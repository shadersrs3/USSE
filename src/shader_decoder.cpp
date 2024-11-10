#include <shader_decoder.h>

namespace betavita::usse {
enum : int {
    SWIZZLE_SEL_X = (0),
    SWIZZLE_SEL_Y = (1),
    SWIZZLE_SEL_Z = (2),
    SWIZZLE_SEL_W = (3),
    SWIZZLE_SEL_0 = (4),
    SWIZZLE_SEL_1 = (5),
    SWIZZLE_SEL_2 = (6),
    SWIZZLE_SEL_HALF = (7),
    SWIZZLE_SEL_UNDEF = (8)
};

enum : int {
    WRITE_MASK_X = (1 << 0),
    WRITE_MASK_Y = (1 << 1),
    WRITE_MASK_Z = (1 << 2),
    WRITE_MASK_W = (1 << 3)
};

#define SWIZZLE(XSEL, YSEL, ZSEL, WSEL) ((SWIZZLE_SEL_##XSEL << (3 * 0)) | \
                                        (SWIZZLE_SEL_##YSEL << (3 * 1)) | \
                                        (SWIZZLE_SEL_##ZSEL << (3 * 2)) | \
                                        (SWIZZLE_SEL_##WSEL << (3 * 3)))

#define SWIZZLE3(SELX, SELY, SELZ) SWIZZLE(SELX, SELY, SELZ, X)

static const uint32_t g_puVec3StdSwizzle[] =
    {
        SWIZZLE3(X, X, X),
        SWIZZLE3(Y, Y, Y),
        SWIZZLE3(Z, Z, Z),
        SWIZZLE3(W, W, W),
        SWIZZLE3(X, Y, Z),
        SWIZZLE3(Y, Z, W),
        SWIZZLE3(X, X, Y),
        SWIZZLE3(X, Y, X),

        SWIZZLE3(Y, Y, X),
        SWIZZLE3(Y, Y, Z),
        SWIZZLE3(Z, X, Y),
        SWIZZLE3(X, Z, Y),
        SWIZZLE3(Y, Z, X),
        SWIZZLE3(Z, Y, X),
        SWIZZLE3(Z, Z, Y),
        SWIZZLE3(X, Y, 1),
};

static const uint32_t g_puVec3ExtSwizzle[] =
    {
        SWIZZLE3(X, Y, Y),
        SWIZZLE3(Y, X, Y),
        SWIZZLE3(X, X, Z),
        SWIZZLE3(Y, X, X),
        SWIZZLE3(X, Y, 0),
        SWIZZLE3(X, 1, 0),
        SWIZZLE3(0, 0, 0),
        SWIZZLE3(1, 1, 1),
        SWIZZLE3(HALF, HALF, HALF),
        SWIZZLE3(2, 2, 2),
        SWIZZLE3(X, 0, 0),
        static_cast<uint32_t>(~0),
        static_cast<uint32_t>(~0),
        static_cast<uint32_t>(~0),
        static_cast<uint32_t>(~0),
        static_cast<uint32_t>(~0),
};

static const uint32_t g_puVec4StdSwizzle[] =
    {
        SWIZZLE(X, X, X, X),
        SWIZZLE(Y, Y, Y, Y),
        SWIZZLE(Z, Z, Z, Z),
        SWIZZLE(W, W, W, W),
        SWIZZLE(X, Y, Z, W),
        SWIZZLE(Y, Z, W, W),
        SWIZZLE(X, Y, Z, Z),
        SWIZZLE(X, X, Y, Z),
        SWIZZLE(X, Y, X, Y),
        SWIZZLE(X, Y, W, Z),
        SWIZZLE(Z, X, Y, W),
        SWIZZLE(Z, W, Z, W),
        SWIZZLE(Y, Z, X, Z),
        SWIZZLE(X, X, Y, Y),
        SWIZZLE(X, Z, W, W),
        SWIZZLE(X, Y, Z, 1),
};

static const uint32_t g_puVec4ExtSwizzle[] =
    {
        SWIZZLE(Y, Z, X, W),
        SWIZZLE(Z, W, X, Y),
        SWIZZLE(X, Z, W, Y),
        SWIZZLE(Y, Y, W, W),
        SWIZZLE(W, Y, Z, W),
        SWIZZLE(W, Z, W, Z),
        SWIZZLE(X, Y, Z, X),
        SWIZZLE(Z, Z, W, W),
        SWIZZLE(X, W, Z, X),
        SWIZZLE(Y, Y, Y, X),
        SWIZZLE(Y, Y, Y, Z),
        SWIZZLE(X, Z, Y, W),
        SWIZZLE(X, X, X, Y),
        SWIZZLE(Z, Y, X, W),
        SWIZZLE(Y, Y, Z, Z),
        SWIZZLE(Z, Z, Z, Y),
};

static const uint32_t g_puScalarStdSwizzle[] =
    {
        SWIZZLE(X, X, X, X),
        SWIZZLE(Y, Y, Y, Y),
        SWIZZLE(Z, Z, Z, Z),
        SWIZZLE(W, W, W, W),
        SWIZZLE(0, 0, 0, 0),
        SWIZZLE(1, 1, 1, 1),
        SWIZZLE(2, 2, 2, 2),
        SWIZZLE(HALF, HALF, HALF, HALF),
        static_cast<uint32_t>(~0),
        static_cast<uint32_t>(~0),
        static_cast<uint32_t>(~0),
        static_cast<uint32_t>(~0),
        static_cast<uint32_t>(~0),
        static_cast<uint32_t>(~0),
        static_cast<uint32_t>(~0),
        static_cast<uint32_t>(~0),
};

static bool secondary_program_state;
static USSEInstructionList usse_instruction_list;
static const uint64_t *gpu_instruction_list;
static int gpu_instruction_length;

static const uint32_t source_modifier_table[4] = {
    SOURCE_MODIFIER_NONE, SOURCE_MODIFIER_NEG,
    SOURCE_MODIFIER_ABS, SOURCE_MODIFIER_NEGABS
};

static int repeat_multiplier[4];
static int repeat_increase[4][17];

void set_secondary_program_state(bool state) {
    secondary_program_state = state;
}

static void reset_sgx543_register(SGX543Register& reg) {
    reg.modifier = 0;
    reg.type = REGTYPE_UNDEF;
    reg.num = 0;
    reg.swizzle_index = 0;
    reg.flags = 0;
}

static void set_repeat_multiplier(const int p0, const int p1, const int p2, const int p3) {
    repeat_multiplier[0] = p0;
    repeat_multiplier[1] = p1;
    repeat_multiplier[2] = p2;
    repeat_multiplier[3] = p3;
}

static void reset_repeat_multiplier() {
    std::fill(repeat_multiplier, repeat_multiplier + 4, 2);
}

static void reset_repeat_increase() {
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 17; j++) {
            repeat_increase[i][j] = j;
        }
    }
}

static void reset_usse_instruction(USSEInstruction& inst) {
    inst.op = OP_UNDEF;
    inst.flags = 0;
    inst.write_mask = 0;
    inst.registers.clear();
}

static void add_usse_instruction(const USSEInstruction& inst) {
    usse_instruction_list.push_back(inst);
}

static void clear_usse_instruction_list() {
    usse_instruction_list.clear();
}

void reset_decoder() {
    reset_repeat_increase();
    reset_repeat_multiplier();
    clear_usse_instruction_list();
    gpu_instruction_list = nullptr;
    gpu_instruction_length = 0;
}

void set_instruction_stream(const uint64_t *instructions, int instruction_length) {
    gpu_instruction_list = instructions;
    gpu_instruction_length = instruction_length;
}

static uint32_t decode_vec34_swizzle(uint32_t uswizzle, uint32_t uext_flag, bool bvec3, bool bvec4) {
    if (bvec3 || bvec4) {
        if (bvec4) {
            if (uext_flag) {
                return g_puVec4ExtSwizzle[uswizzle];
            }
            else {
                return g_puVec4StdSwizzle[uswizzle];
            }
        }
        else {
            if (uext_flag) {
                return g_puVec3ExtSwizzle[uswizzle];
            } else {
                return g_puVec3StdSwizzle[uswizzle];
            }
        }
    } else {
        if (uext_flag) {
            return -1;
        }
        else {
            return g_puScalarStdSwizzle[uswizzle];
        }
    }
    return -1;
}

static int get_repeat_offset_moe(uint8_t repeat_index, int dstsrcindex) {
    auto inc = repeat_increase[dstsrcindex][repeat_index];
    inc *= repeat_multiplier[dstsrcindex];
    return inc;
}

static void adjust_register_number_units(const USSERegType& reg_type, int *num, bool double_registers) {
    if (double_registers) {
        if (reg_type != REGTYPE_IMMEDIATE && reg_type != REGTYPE_SPECIAL)
            *num <<= 1;
    }
}

// This is for secondary programs at the start of execution, primary programs don't have to change it back to pa, o, etc.
static void compute_register_final_pass(USSERegType& reg_bank) {
    if (secondary_program_state &&
        reg_bank != REGTYPE_FPINTERNAL && reg_bank != REGTYPE_FPCONSTANT && reg_bank != REGTYPE_IMMEDIATE)
        reg_bank = REGTYPE_SECATTR;
}

static void perform_special_register_renaming(USSERegType& reg_bank, int *num) {
    int regnum = *num;

    if (reg_bank == REGTYPE_SPECIAL) {
        if (regnum & 0x40) {
            regnum &= ~0x40;
            reg_bank = REGTYPE_GLOBAL;
        } else
            reg_bank = REGTYPE_FPCONSTANT;
    }
}

static void perform_temp_register_renaming(bool double_registers, USSERegType& reg_bank, int *num, int num_field_length, int index_reg_num = 0) {
    uint32_t num_temps_mapped;
    uint32_t max_reg_num;
    int _num_value = *num;

    num_temps_mapped = 4;
    if (double_registers) {
        num_temps_mapped <<= 1;
    }

    max_reg_num = 1 << num_field_length;

    if (reg_bank == REGTYPE_TEMP && _num_value >= (max_reg_num - num_temps_mapped)) {
        reg_bank = REGTYPE_FPINTERNAL;
        _num_value -= (max_reg_num - num_temps_mapped);
        if (double_registers) {
            _num_value >>= 1;
        }
    }

    *num = _num_value;
}

static USSERegType decode_src0_bank(uint32_t upper_instruction, bool allow_extended, int num, int num_field_length, uint32_t uext) {
    static USSERegType register_type[2] = { REGTYPE_TEMP, REGTYPE_PRIMATTR };
    static USSERegType extended_register_type[2] = { REGTYPE_OUTPUT, REGTYPE_SECATTR };
    int bank_nr;
    bool extended_bank;

    bank_nr = (upper_instruction & ~0xFFFFFFFBU) >> 2;
    extended_bank = allow_extended && (upper_instruction & uext);

    if (bank_nr >= 2) {
        printf("Can't decode src0! invalid bank nr %d", bank_nr);
        return REGTYPE_UNDEF;
    }
    return extended_bank ? extended_register_type[bank_nr] : register_type[bank_nr];
}

static USSERegType decode_src0_with_num(bool double_registers, uint32_t upper_instruction, bool allow_extended, int *num, int num_field_length, uint32_t uext) {
    USSERegType reg;

    reg = decode_src0_bank(upper_instruction, allow_extended, *num, num_field_length, uext);
    adjust_register_number_units(reg, num, double_registers);
    perform_special_register_renaming(reg, num);
    perform_temp_register_renaming(double_registers, reg, num, num_field_length, 0);
    compute_register_final_pass(reg);
    return reg;
}

static USSERegType decode_src12_bank(uint32_t upper_instruction, uint32_t lower_instruction, bool allow_extended, int src_num, int num_field_length, uint32_t uext) {
    int bank_nr;
    bool extended_bank;
    
    static USSERegType register_type[4] = { REGTYPE_TEMP, REGTYPE_OUTPUT, REGTYPE_PRIMATTR, REGTYPE_SECATTR };
    static USSERegType extended_register_type[4] = { REGTYPE_INDEXED_LOW, REGTYPE_SPECIAL, REGTYPE_IMMEDIATE, REGTYPE_INDEXED_HIGH };

    switch (src_num) {
    default:
        [[fallthrough]];
    case 1:
        bank_nr = (lower_instruction & ~0x3FFFFFFF) >> 30;
        break;
    case 2:
        bank_nr = (lower_instruction & ~0xCFFFFFFF) >> 28;
        break;
    }

    if (allow_extended && (upper_instruction & uext) == uext)
        extended_bank = true;
    else
        extended_bank = false;

    return extended_bank ? extended_register_type[bank_nr] : register_type[bank_nr];
}

static USSERegType decode_src12_with_num(bool double_registers, uint32_t upper_instruction, uint32_t lower_instruction, int src_num, bool allow_extended, int *num, int num_field_length, uint32_t uext) {
    USSERegType reg;

    reg = decode_src12_bank(upper_instruction, lower_instruction, true, src_num, num_field_length, uext);

    adjust_register_number_units(reg, num, double_registers);
    perform_special_register_renaming(reg, num);
    perform_temp_register_renaming(double_registers, reg, num, num_field_length, 0);
    compute_register_final_pass(reg);
    return reg;
}

static USSERegType decode_dest_bank(int bank_nr, bool extended_bank) {
    static USSERegType register_type[4] = { REGTYPE_TEMP, REGTYPE_OUTPUT, REGTYPE_PRIMATTR, REGTYPE_INDEXED_LOW };
    static USSERegType extended_register_type[4] = { REGTYPE_SECATTR, REGTYPE_SPECIAL, REGTYPE_INDEX, REGTYPE_INDEXED_HIGH };

    if (bank_nr >= 4)
        return REGTYPE_UNDEF;

    return extended_bank ? extended_register_type[bank_nr] : register_type[bank_nr];
}

static USSERegType decode_dest_with_num(bool double_registers, uint32_t upper_instruction, bool allow_extended, int *num, int num_field_length) {
    int bank_nr;
    bool extended_bank;
    USSERegType reg;

    bank_nr = (upper_instruction & ~0xFFFFFFFC);
    if (allow_extended && (upper_instruction & 0x00080000) == 0x00080000) {
        extended_bank = true;
    } else {
        extended_bank = false;
    }

    reg = decode_dest_bank(bank_nr, extended_bank);
    adjust_register_number_units(reg, num, double_registers);
    perform_special_register_renaming(reg, num);
    perform_temp_register_renaming(double_registers, reg, num, num_field_length, 0);
    compute_register_final_pass(reg);
    return reg;
}

static uint32_t decode_vec_destination_write_mask(const USSERegType& reg, bool bf32, bool bf16_masks_unrestricted, uint32_t encoded_write_mask) {
    uint32_t decoded_write_mask;
    if (reg != REGTYPE_TEMP &&
        reg != REGTYPE_PRIMATTR &&
        reg != REGTYPE_OUTPUT &&
        reg != REGTYPE_SECATTR &&
        reg != REGTYPE_FPINTERNAL) {
        bf16_masks_unrestricted = false;
    }

    if (reg == REGTYPE_FPINTERNAL || (!bf32 && bf16_masks_unrestricted)) {
        decoded_write_mask = 0;
        if (encoded_write_mask & 0x1) {
            decoded_write_mask |= WRITE_MASK_X;
        }
        if (encoded_write_mask & 0x2) {
            decoded_write_mask |= WRITE_MASK_Y;
        }
        if (encoded_write_mask & 0x4) {
            decoded_write_mask |= WRITE_MASK_Z;
        }
        if (encoded_write_mask & 0x8) {
            decoded_write_mask |= WRITE_MASK_W;
        }
    } else {
        if (bf32) {
            decoded_write_mask = 0;
            if (encoded_write_mask & 1) {
                decoded_write_mask |= WRITE_MASK_X;
            }

            if (encoded_write_mask & 2) {
                decoded_write_mask |= WRITE_MASK_Y;
            }
        } else {
            decoded_write_mask = 0;
            if (encoded_write_mask & 1)
            {
                decoded_write_mask |= WRITE_MASK_X | WRITE_MASK_Y;
            }
            if (encoded_write_mask & 4)
            {
                decoded_write_mask |= WRITE_MASK_Z | WRITE_MASK_W;
            }
        }
    }
    return decoded_write_mask;
}

static void decode_vec4mad_instruction(int upper_part, int lower_part) {
    static const uint32_t auSrc0Swizzle[] = {
        SWIZZLE(X, X, X, X),
        SWIZZLE(Y, Y, Y, Y),
        SWIZZLE(Z, Z, Z, Z),
        SWIZZLE(W, W, W, W),
        SWIZZLE(X, Y, Z, W),
        SWIZZLE(Y, Z, X, W),
        SWIZZLE(X, Y, W, W),
        SWIZZLE(Z, W, X, Y),
    };
    static const uint32_t auSrc1Swizzle[] = {
        SWIZZLE(X, X, X, X),
        SWIZZLE(Y, Y, Y, Y),
        SWIZZLE(Z, Z, Z, Z),
        SWIZZLE(W, W, W, W),
        SWIZZLE(X, Y, Z, W),
        SWIZZLE(X, Y, Y, Z),
        SWIZZLE(Y, Y, W, W),
        SWIZZLE(W, Y, Z, W) 
    };
    static const uint32_t auSrc2Swizzle[] = {
        SWIZZLE(X, X, X, X),
        SWIZZLE(Y, Y, Y, Y),
        SWIZZLE(Z, Z, Z, Z),
        SWIZZLE(W, W, W, W),
        SWIZZLE(X, Y, Z, W),
        SWIZZLE(X, Z, W, W),
        SWIZZLE(X, X, Y, Z),
        SWIZZLE(X, Y, Z, Z),
    };

    USSEInstruction inst;
    SGX543Register reg[4];
    bool is_f16 = (upper_part & 0x04000000) == 0x04000000; // vf16mad
    int dest_num, src0_num, src1_num, src2_num, src0_swizzle, src1_swizzle, src2_swizzle;
    uint32_t hw_dest_mask;
    reset_usse_instruction(inst);
    for (int i = 0; i < 4; i++)
        reset_sgx543_register(reg[i]);

    inst.op = is_f16 ? OP_VF16MAD : OP_V4MAD;
    inst.flags = 0;
    inst.write_mask = 0;

    // decode dest register
    dest_num = (lower_part & ~0xF03FFFFF) >> 22;
    hw_dest_mask = (upper_part & ~0xFFFFF87F) >> 7;

    reg[0].type = decode_dest_with_num(true, upper_part, true, &dest_num, 7);
    reg[0].num = dest_num;
    reg[0].swizzle_index = 0;
    reg[0].modifier = SOURCE_MODIFIER_NONE;
    inst.write_mask = decode_vec_destination_write_mask(reg[0].type, !is_f16, false, hw_dest_mask);

    // decode src0 register
    src0_num = (lower_part & ~0xFFFC0FFF) >> 12;
    src0_swizzle = ((lower_part & ~0xFFF3FFFF) >> 18) | ((upper_part & ~0xFFDFFFFF) >> 21) << 2;
    reg[1].type = decode_src0_with_num(true, upper_part, false, &src0_num, 7, 0);
    reg[1].num = src0_num;

    if ((upper_part & 0x40000) != 0)
        reg[1].modifier = SOURCE_MODIFIER_ABS;

    reg[1].swizzle_index = auSrc0Swizzle[src0_swizzle];

    // decode src1 register
    src1_num = (lower_part & ~0xFFFFF03F) >> 6;
    src1_swizzle = ((lower_part & ~0xFFCFFFFF) >> 20) | ((upper_part & ~0xFFFFEFFF) >> 12) << 2;
    reg[2].type = decode_src12_with_num(true, upper_part, lower_part, 1, true, &src1_num, 7, 0x00020000);
    reg[2].num = src1_num;
    reg[2].modifier = source_modifier_table[(upper_part & ~0xFFFFFF9F) >> 5];
    reg[2].swizzle_index = auSrc1Swizzle[src1_swizzle];

    src2_num = (lower_part & ~0xFFFFFFC0);
    src2_swizzle = ((upper_part & ~0xFFFF1FFF) >> 13);
    reg[3].type = decode_src12_with_num(true, upper_part, lower_part, 2, true, &src2_num, 7, 0x00010000);
    reg[3].num = src2_num;
    reg[3].modifier = source_modifier_table[(upper_part & ~0xFFFFFFE7) >> 3];
    reg[3].swizzle_index = auSrc2Swizzle[src2_swizzle];

    for (int i = 0; i < 4; i++)
        inst.registers.push_back(reg[i]);
    add_usse_instruction(inst);
}

static int decode_vec4nmad_src1_swizzle(int lower_part, int upper_part) {
    uint32_t hw_swizzle;
    uint32_t chan;
    int swizzle;

	hw_swizzle =
		((lower_part & ~0xFFC07FFF) >> 15) << 0;
	hw_swizzle |=
		((upper_part & ~0xFFFFFFF3) >> 2) << 7;
	hw_swizzle |=
		((upper_part & ~0xFFFBFFFF) >> 18) << 9;
	hw_swizzle |=
		((upper_part & ~0xFF9FFFFF) >> 21) << 10;

    swizzle = 0;
    for (chan = 0; chan < 4; chan++) {
        uint32_t sel;
        static const uint32_t swizzle_selector_table[] = {
            SWIZZLE_SEL_X,
            SWIZZLE_SEL_Y,
            SWIZZLE_SEL_Z,
            SWIZZLE_SEL_W,
            SWIZZLE_SEL_0,
            SWIZZLE_SEL_1,
            SWIZZLE_SEL_2,
            SWIZZLE_SEL_HALF
        };

        switch (chan) {
        case 0: sel = (hw_swizzle & ~0xFF8) >> 0; break;
        case 1: sel = (hw_swizzle & ~0xFC7) >> 3; break;
        case 2: sel = (hw_swizzle & ~0xE3F) >> 6; break;
        case 3: sel = (hw_swizzle & ~0x1FF) >> 9; break;
        }

        swizzle |= swizzle_selector_table[sel] << (3 * chan);
    }
    return swizzle;
}

static void decode_vecnmad_instruction(int upper_part, int lower_part, bool bf16) {
    USSEInstruction inst;
    SGX543Register reg[3];

    static const USSEOpcodeType decode_f16_table[] = {
        OP_VF16MUL,
        OP_VF16ADD,
        OP_VF16FRC,
        OP_VF16DSX,
        OP_VF16DSY,
        OP_VF16MIN,
        OP_VF16MAX,
        OP_VF16DP,
    };

    static const USSEOpcodeType decode_f32_table[] = {
        OP_VMUL,
        OP_VADD,
        OP_VFRC,
        OP_VDSX,
        OP_VDSY,
        OP_VMIN,
        OP_VMAX,
        OP_VDP,
    };

    static const uint32_t auSrc2Swizzle[] = {
        SWIZZLE(X, X, X, X),
        SWIZZLE(Y, Y, Y, Y),
        SWIZZLE(Z, Z, Z, Z),
        SWIZZLE(W, W, W, W),
        SWIZZLE(X, Y, Z, W),
        SWIZZLE(Y, Z, W, W),
        SWIZZLE(X, Y, Z, Z),
        SWIZZLE(X, X, Y, Z),
        SWIZZLE(X, Y, X, Y),
        SWIZZLE(X, Y, W, Z),
        SWIZZLE(Z, X, Y, W),
        SWIZZLE(Z, W, Z, W),
        SWIZZLE(Y, Z, X, Z),
        SWIZZLE(X, X, Y, Y),
        SWIZZLE(X, Z, W, W),
        SWIZZLE(X, Y, Z, 1),
    };

    int dest_num, src1_num, src2_num;
    int src1_modifier, src1_swizzle, src2_swizzle;
    int write_mask;

    reset_usse_instruction(inst);
    for (int i = 0; i < 3; i++)
        reset_sgx543_register(reg[i]);

    int opcode = (lower_part & ~0xFFFF8FFF) >> 12;
    inst.op = bf16 ? decode_f16_table[opcode] : decode_f32_table[opcode];
    inst.write_mask = (upper_part & ~0xFFFFF87F) >> 7;
    dest_num = (lower_part & ~0xF03FFFFF) >> 22;

    reg[0].type = decode_dest_with_num(true, upper_part, true, &dest_num, 7);
    reg[0].num = dest_num;

    src1_num = (lower_part & ~0xFFFFF03F) >> 6;
    reg[1].type = decode_src12_with_num(true, upper_part, lower_part, 1, true, &src1_num, 7, 0x00020000);
    reg[1].num = src1_num;
    reg[1].modifier = source_modifier_table[(upper_part & ~0xFFFFFF9FU) >> 5];
    reg[1].swizzle_index = decode_vec4nmad_src1_swizzle(lower_part, upper_part);

    src2_num = (lower_part & ~0xFFFFFFC0U);
    reg[2].type = decode_src12_with_num(true, upper_part, lower_part, 2, true, &src2_num, 7, 0x10000);
    reg[2].num = src2_num;
    if (upper_part & 0x10)
        reg[2].modifier = SOURCE_MODIFIER_ABS;
        reg[2].swizzle_index = auSrc2Swizzle[(upper_part & ~0xFFFF0FFFU) >> 12];

    for (int i = 0; i < 3; i++)
        inst.registers.push_back(reg[i]);
    add_usse_instruction(inst);
}

static void decode_vecmov_instruction(int upper_part, int lower_part) {
    USSEInstruction inst;
    SGX543Register reg[4];

    static const USSEOpcodeType move_op_table[] = { OP_VMOV, OP_VMOVC, OP_VMOVCU8, OP_UNDEF };
    int hw_write_mask;
    int move_type = (upper_part & ~0xFFFF3FFF) >> 14;
    int data_type;
    bool double_registers;
    int number_field_length;
    int repeat_count;
    int current_arg = 0;

    reset_usse_instruction(inst);

    inst.op = move_op_table[move_type];

    data_type = (upper_part & ~0xFFFFF8FF) >> 8;
    static const uint32_t data_type_flag_table[] = { MOVE_TYPE_INT8, MOVE_TYPE_INT16, MOVE_TYPE_INT32, MOVE_TYPE_C10, MOVE_TYPE_F16, MOVE_TYPE_F32 };
    if (data_type < sizeof data_type_flag_table / sizeof *data_type_flag_table) {
        inst.flags = data_type_flag_table[data_type];
    } else {
        printf("Invalid vecmov data type!\n");
        abort();
    }

    if (data_type == DATA_TYPE_C10 || data_type == DATA_TYPE_F16 || data_type == DATA_TYPE_F32) {
        double_registers = true;
        number_field_length = 7;
    } else {
        double_registers = false;
        number_field_length = 6;
    }

    if (inst.op != OP_VMOV) {
        for (int i = 0; i < 4; i++)
            reset_sgx543_register(reg[i]);

        printf("unimplemented conditional moves!\n");
        abort();
    } else {
        reset_sgx543_register(reg[0]);
        reset_sgx543_register(reg[1]);
    }

    int dest_num = (lower_part & ~0xFF03FFFF) >> 18;
    
    reg[0].type = decode_dest_with_num(double_registers, upper_part, true, &dest_num, 7);
    reg[0].num = dest_num;


    hw_write_mask = (lower_part & ~0xF0FFFFFF) >> 24;
    if (data_type == DATA_TYPE_F32 || data_type == DATA_TYPE_F16) {
        bool bf32;

        bf32 = data_type == DATA_TYPE_F32;
        inst.write_mask = decode_vec_destination_write_mask(reg[0].type, bf32, false, hw_write_mask);
    } else {
        inst.write_mask = hw_write_mask;
    }

    // vecmov conditional

    current_arg++;

    int src1_num = (lower_part & ~0xFFFFF03F) >> 6;
    reg[current_arg].type = decode_src12_with_num(true, upper_part, lower_part, 1, true, &src1_num, number_field_length, 0x00020000U);
    reg[current_arg].num = src1_num;

    current_arg++;

    repeat_count = ((upper_part & ~0xFFFFCFFF) >> 12) + 1;
    for (int i = 0; i < repeat_count; i++) {
        int dest_repeat_offset = get_repeat_offset_moe(i, 3);
        int src1_repeat_offset = get_repeat_offset_moe(i, 1);

        if (inst.op == OP_VMOV) {
            reg[0].num = dest_num + dest_repeat_offset;
            reg[1].num = src1_num + src1_repeat_offset;

            inst.registers.push_back(reg[0]);
            inst.registers.push_back(reg[1]);
        } else {
            printf("unimplemented vmovc!\n");
            abort();
        }

        add_usse_instruction(inst);
        inst.registers.clear();
    }
}

void decode_instruction_stream() {
    bool bad_opcode = false;
    for (int i = 0; i < gpu_instruction_length; i++) {
        int common_opcode;
        
        if (bad_opcode)
            break;

        int upper_part = (int) (*(gpu_instruction_list + i) >> 32);
        int lower_part = (int) *(gpu_instruction_list + i);

        common_opcode = ((*(gpu_instruction_list + i) >> 32) & ~0x07FFFFFF) >> 27;

        if (i > 3)
            secondary_program_state = false;
        switch (common_opcode) {
        case 0: // vec4mad
        {
            decode_vec4mad_instruction(upper_part, lower_part);
            break;
        }
        case 1: // vecnmadf32
        {
            decode_vecnmad_instruction(upper_part, lower_part, false);
            break;
        }
        case 2: // vecnmadf16
        {
            decode_vecnmad_instruction(upper_part, lower_part, true);
            break;
        }
        case 3: // svec
        {
            USSEInstruction inst;
            SGX543Register reg[4];
            int write_mask;
            int dest_num, src0_num, src1_num, src2_num;
            int increment_mode = (upper_part & ~0xFFFE7FFFU) >> 15;
            int repeat_count = (upper_part & ~0xFFFFCFFFU) >> 12;

            static const USSEOpcodeType ops[] = {
                OP_VDP3,
                OP_VDP4,
                OP_VMAD3,
                OP_VMAD4
            };

            reset_usse_instruction(inst);
            for (int i = 0; i < 4; i++)
                reset_sgx543_register(reg[i]);

            USSEOpcodeType op = ops[(upper_part & ~0xFFCFFFFFU) >> 20];
            write_mask = (upper_part & ~0xFFFFF87FU) >> 7;
            dest_num = (lower_part & ~0xF03FFFFFU) >> 22;
            src1_num = (lower_part & ~0xFFFFFFC0U);

            inst.op = op;
            inst.write_mask = write_mask;

            reg[0].type = decode_dest_with_num(true, upper_part, true, &dest_num, 7);
            reg[0].num = dest_num;

            reg[2].type = decode_src12_with_num(true, upper_part, lower_part, 1, true, &src1_num, 7, 0x00020000);
            reg[2].num = src1_num;

            if (upper_part & 0x40)
                reg[2].modifier = SOURCE_MODIFIER_NEG;

            if (upper_part & 0x20)
                reg[2].modifier |= SOURCE_MODIFIER_ABS;


            bool bvec4 = op == OP_VMAD4 || op == OP_VDP4;
            switch (op) {
            case OP_VDP3:
            case OP_VDP4:
                printf("VDP instruction unimplemented!");
                abort();
                break;
            case OP_VMAD3:
            case OP_VMAD4:
            {
                // decode first source swizzle (src1)
                {
                    uint32_t us_swiz;
                    bool us_swiz_ext_flag;
                    uint32_t swizzle_encoding;
                
                    us_swiz = (lower_part & ~0xFFFFFC3FU) >> 6;
                    us_swiz_ext_flag = (lower_part & 0x00000400U) != 0;
                    swizzle_encoding = decode_vec34_swizzle(us_swiz, us_swiz_ext_flag, !bvec4, bvec4);
                    reg[2].swizzle_index = swizzle_encoding;
                }

                // gpi0
                reg[1].type = REGTYPE_FPINTERNAL;
                reg[1].num = (lower_part & ~0xCFFFFFFFU) >> 28;

                if (upper_part & 0x00004000) {
                    reg[1].modifier = SOURCE_MODIFIER_ABS;
                }

                // decode second source swizzle (gpi0)
                {
                    uint32_t gpi0_hw_swiz = (lower_part & ~0xFFC3FFFFU) >> 18;
                    bool gpi0_swiz_extended_flag = (upper_part & 0x4) != 0;

                    reg[1].swizzle_index = decode_vec34_swizzle(gpi0_hw_swiz, gpi0_swiz_extended_flag, !bvec4, bvec4);
                }

                // gpi1
                reg[3].type = REGTYPE_FPINTERNAL;
                reg[3].num = (lower_part & ~0xFFFFCFFFU) >> 12;

                if (upper_part & 0x10)
                    reg[3].modifier |= SOURCE_MODIFIER_NEG;

                if (upper_part & 0x8)
                    reg[3].modifier |= SOURCE_MODIFIER_ABS;

                // decode third source swizzle (gpi1)
                {
                    uint32_t gpi1_hw_swiz = (lower_part & ~0xFFFC3FFFU) >> 14;
                    bool gpi1_swiz_extended_flag = (upper_part & 0x00400000U) != 0;
                    reg[3].swizzle_index = decode_vec34_swizzle(gpi1_hw_swiz, gpi1_swiz_extended_flag, !bvec4, bvec4);
                }

                for (int i = 0; i < 4; i++)
                    inst.registers.push_back(reg[i]);
                usse_instruction_list.push_back(inst);
                break;
            }

            default:
                abort();
            }
            break;
        }
        case 7: // vecmov
        {
            decode_vecmov_instruction(upper_part, lower_part);
            break;
        }
        case 8: // vecpck
        {
            static const USSEOpcodeType fmt_ops[][8] = {
                /* PCK_FMT_U8 */
                {
                    OP_VPCKU8U8,
                    OP_VPCKU8S8,
                    OP_VPCKU8O8,
                    OP_VPCKU8U16,
                    OP_VPCKU8S16,
                    OP_VPCKU8F16,
                    OP_VPCKU8F32,
                    OP_UNDEF,
                },
                /* PCK_FMT_S8 */
                {
                    OP_VPCKS8U8,
                    OP_VPCKS8S8,
                    OP_VPCKS8O8,
                    OP_VPCKS8U16,
                    OP_VPCKS8S16,
                    OP_VPCKS8F16,
                    OP_VPCKS8F32,
                    OP_UNDEF
                },
                /* PCK_FMT_O8 */
                {
                    OP_VPCKO8U8,
                    OP_VPCKO8S8,
                    OP_VPCKO8O8,
                    OP_VPCKO8U16,
                    OP_VPCKO8S16,
                    OP_VPCKO8F16,
                    OP_VPCKO8F32,
                    OP_UNDEF,
                },
                /* PCK_FMT_U16 */
                {
                    OP_VPCKU16U8,
                    OP_VPCKU16S8,
                    OP_VPCKU16O8,
                    OP_VPCKU16U16,
                    OP_VPCKU16S16,
                    OP_VPCKU16F16,
                    OP_VPCKU16F32,
                    OP_UNDEF,
                },
                /* PCK_FMT_S16 */
                {
                    OP_VPCKS16U8,
                    OP_VPCKS16S8,
                    OP_VPCKS16O8,
                    OP_VPCKS16U16,
                    OP_VPCKS16S16,
                    OP_VPCKS16F16,
                    OP_VPCKS16F32,
                    OP_UNDEF
                },
                /* PCK_FMT_F16 */
                {
                    OP_VPCKF16U8,
                    OP_VPCKF16S8,
                    OP_VPCKF16O8,
                    OP_VPCKF16U16,
                    OP_VPCKF16S16,
                    OP_VPCKF16F16,
                    OP_VPCKF16F32,
                    OP_VPCKF16C10,
                },
                /* PCK_FMT_F32 */
                {
                    OP_VPCKF32U8,
                    OP_VPCKF32S8,
                    OP_VPCKF32O8,
                    OP_VPCKF32U16,
                    OP_VPCKF32S16,
                    OP_VPCKF32F16,
                    OP_VPCKF32F32,
                    OP_VPCKF32C10,
                },
                /* PCK_FMT_C10 */
                {
                    OP_UNDEF,
                    OP_UNDEF,
                    OP_UNDEF,
                    OP_UNDEF,
                    OP_UNDEF,
                    OP_VPCKC10F16,
                    OP_VPCKC10F32,
                    OP_VPCKC10C10,
                },
            };

            USSEInstruction inst;
            SGX543Register reg[3];
            reset_usse_instruction(inst);

            for (int i = 0; i < 3; i++)
                reset_sgx543_register(reg[i]);

            uint32_t dest_mask = (upper_part & ~0xFFFFFFC3U) >> 2;
            uint32_t dest_fmt = (upper_part & ~0xFFFFFE3FU) >> 6;
            uint32_t src_fmt = (upper_part & ~0xFFFFF1FFU) >> 9;
            uint32_t write_mask = (upper_part & ~0xFFFFFFC3U) >> 2;

            int repeat_count = (upper_part & ~0xFFFF0FFFU) >> 12;
            uint32_t channel;
            uint32_t swizzle_src;
            bool replicate_conversion;
            bool src_float, dest_float;
            bool sparse_swizzle;
            uint32_t swizzle_sel[4];

            USSEOpcodeType opcode = fmt_ops[dest_fmt][src_fmt];

            inst.op = opcode;
            inst.write_mask = (int) write_mask;
            int dest_num = (lower_part & ~0xF01FFFFFU) >> 21;

            reg[0].type = decode_dest_with_num(false, upper_part, true, &dest_num, 7);
            reg[0].num = dest_num;

            inst.registers.push_back(reg[0]);

            swizzle_src = 2;

            if (src_fmt == 6 || src_fmt == 5 || src_fmt == 7) {
                int src1_num = (lower_part & ~0xFFFFC0FFU) >> 8;
                reg[1].type = decode_src12_with_num(true, upper_part, lower_part, 1, true, &src1_num, 7, 0x00020000);
                reg[1].num = src1_num;

                inst.registers.push_back(reg[1]);
                if (src_fmt == 6) {
                    int src2_num = (lower_part & ~0xFFFFFF81U) >> 1;
                    reg[2].type = decode_src12_with_num(true, upper_part, lower_part, 1, true, &src2_num, 7, 0x00020000);
                    reg[2].num = src2_num;
                    inst.registers.push_back(reg[2]);
                }
            }

            usse_instruction_list.push_back(inst);
            break;
        }
        case 31: // special
        {
            int op = (upper_part & ~0xFFCFFFFFU) >> 20;

            switch (op) {
            case 0:
                printf("spec (flowctrl)\n");
                break;
            case 1: // moectrl
            {
                int ops[] = {
                    OP_SMOA,
                    OP_SMR,
                    OP_SMLSI,
                    OP_SMBO,
                    OP_IMO,
                    OP_SETFC
                };

                int opcode = ops[(upper_part & ~0xF8FFFFFFU) >> 24];

                switch (opcode) {
                case OP_SMLSI:
                {
                    USSEInstruction inst;

                    reset_usse_instruction(inst);

                    inst.op = OP_SMLSI;

                    int dest_inc_mode = (upper_part >> 3) & 1;
                    int src0_inc_mode = (upper_part >> 2) & 1, src1_inc_mode = (upper_part >> 1) & 1, src2_inc_mode = upper_part & 1;
                    int dest_inc = (lower_part >> 24) & 0xFF, src0_inc = (lower_part >> 16) & 0xFF, src1_inc = (lower_part >> 8) & 0xFF, src2_inc = lower_part & 0xFF;
                    auto parse_increment = [&](const int idx, const int inc_mode, const uint8_t inc_value) {
                        if (inc_mode) {
                            // Parse value as swizzle
                            for (int i = 0; i < 4; i++) {
                                repeat_increase[idx][i] = ((inc_value >> (2 * i)) & 0b11);
                            }
                        } else {
                            // Parse value as immediate
                            for (int i = 0; i < 17; i++) {
                                repeat_increase[idx][i] = i * static_cast<std::int8_t>(inc_value);
                            }
                        }
                    };

                    parse_increment(3, dest_inc_mode, dest_inc);
                    parse_increment(0, src0_inc_mode, src0_inc);
                    parse_increment(1, src1_inc_mode, src1_inc);
                    parse_increment(2, src2_inc_mode, src2_inc);
                    break;
                }
                default:
                    printf("unimplemented spec (moectrl opcode)\n");
                    break;
                }
                break;
            }
            case 2: // other
                printf("spec (other)\n");
                break;
            case 3: // vistest
                printf("spec (vistest)\n");
                break;
            }
            break;
        }
        default:
            printf("Unknown opcode %d\n", common_opcode);
            bad_opcode = true;
            break;
        }
    }
}

USSEInstructionList get_instruction_list() {
    return usse_instruction_list;
}
}