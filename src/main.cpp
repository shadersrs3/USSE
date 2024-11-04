#include <cstdio>
#include <cstdint>
#include <cstdlib>

#include <algorithm>

enum USSEOpcodeType : int {
    OP_VMOV,
    OP_VMOVC,
    OP_VMOVCU8,
    OP_VF16MAD,
    OP_VMAD,

    OP_VPCKU8U8,
    OP_VPCKU8S8,
    OP_VPCKU8O8,
    OP_VPCKU8U16,
    OP_VPCKU8S16,
    OP_VPCKU8F16,
    OP_VPCKU8F32,
    OP_VPCKS8U8,
    OP_VPCKS8S8,
    OP_VPCKS8O8,
    OP_VPCKS8U16,
    OP_VPCKS8S16,
    OP_VPCKS8F16,
    OP_VPCKS8F32,
    OP_VPCKO8U8,
    OP_VPCKO8S8,
    OP_VPCKO8O8,
    OP_VPCKO8U16,
    OP_VPCKO8S16,
    OP_VPCKO8F16,
    OP_VPCKO8F32,
    OP_VPCKU16U8,
    OP_VPCKU16S8,
    OP_VPCKU16O8,
    OP_VPCKU16U16,
    OP_VPCKU16S16,
    OP_VPCKU16F16,
    OP_VPCKU16F32,
    OP_VPCKS16U8,
    OP_VPCKS16S8,
    OP_VPCKS16O8,
    OP_VPCKS16U16,
    OP_VPCKS16S16,
    OP_VPCKS16F16,
    OP_VPCKS16F32,
    OP_VPCKF16U8,
    OP_VPCKF16S8,
    OP_VPCKF16O8,
    OP_VPCKF16U16,
    OP_VPCKF16S16,
    OP_VPCKF16F16,
    OP_VPCKF16F32,
    OP_VPCKF16C10,
    OP_VPCKF32U8,
    OP_VPCKF32S8,
    OP_VPCKF32O8,
    OP_VPCKF32U16,
    OP_VPCKF32S16,
    OP_VPCKF32F16,
    OP_VPCKF32F32,
    OP_VPCKF32C10,
    OP_VPCKC10F16,
    OP_VPCKC10F32,
    OP_VPCKC10C10,
    
    OP_UNDEF,
};

enum USSERegType : int {
    REGTYPE_TEMP,
    REGTYPE_PRIMATTR,
    REGTYPE_OUTPUT,
    REGTYPE_SECATTR,
    REGTYPE_FPINTERNAL,
    REGTYPE_SPECIAL,
    REGTYPE_GLOBAL,
    REGTYPE_FPCONSTANT,
    REGTYPE_IMMEDIATE,
    REGTYPE_INDEX,
    // both are INDEXED types
    REGTYPE_INDEXED_LOW,
    REGTYPE_INDEXED_HIGH,
    // **********************
    REGTYPE_UNDEF,
    
};

enum USSEDataType : int {
    DATA_TYPE_INT8,
    DATA_TYPE_INT16,
    DATA_TYPE_INT32,
    DATA_TYPE_C10,
    DATA_TYPE_F16,
    DATA_TYPE_F32,
};

enum USSEIndexReg : int {
    INDEXREG_UNDEF,
    INDEXREG_LOW,
    INDEXREG_HIGH
};

const char *get_register_type_name(const USSERegType& reg) {
    static const char *reg_name[] = {
        "temp", "primattr", "output", "secattr",
        "fpinternal", "special", "global", "fpconstant",
        "immediate", "index", "indexedlow", "indexedhigh"
    };

    if (reg < 0 || reg >= REGTYPE_UNDEF)
        return "undef";
    return reg_name[reg];
}

const char *get_register_operand_name(const USSERegType& reg) {
    static const char *reg_name[] = {
        "r", "pa", "o", "sa",
        "i", "badspecial", "g", "c",
        "#", "index", "indexed", "indexedlow", "indexedhigh"
    };

    if (reg < 0 || reg >= REGTYPE_UNDEF)
        return "undef";
    return reg_name[reg];
}

const char *get_source_modifier_name(int index) {
    static const char *source_modifier_name_table[] = {
        "",
        ".neg",
        ".abs",
        ".negabs",
    };
    return source_modifier_name_table[index % 4];
}

const char *get_vecmad_source_swizzle(int src_num, int index) {
	static const char *swizzle_table[3][8] = 
	{
        {
            "xxxx",
            "yyyy",
            "zzzz",
            "wwww",
            "xyzw",
            "yzxw",
            "xyww",
            "zwxy"
        },
        {
            "xxxx",
            "yyyy",
            "zzzz",
            "wwww",
            "xyzw",
            "xyyz",
            "yyww",
            "wyzw"
        },
        {
            "xxxx",
            "yyyy",
            "zzzz",
            "wwww",
            "xyzw",
            "xzww",
            "xxyz",
            "xyzz"
        }
    };
    return swizzle_table[src_num][index];
}

bool secondary_program_state = false;

int repeat_multiplier[4];
int repeat_increase[4][17];

void set_repeat_multiplier(const int p0, const int p1, const int p2, const int p3) {
    repeat_multiplier[0] = p0;
    repeat_multiplier[1] = p1;
    repeat_multiplier[2] = p2;
    repeat_multiplier[3] = p3;
}

void reset_repeat_multiplier() {
    std::fill(repeat_multiplier, repeat_multiplier + 4, 2);
}

void reset_repeat_increase() {
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 17; j++) {
            repeat_increase[i][j] = j;
        }
    }
}

int get_repeat_offset(const USSERegType& reg_type, uint8_t repeat_index, int dstsrcindex) {
    auto inc = repeat_increase[dstsrcindex][repeat_index];

    inc *= repeat_multiplier[dstsrcindex];
    return inc;
}


void adjust_register_number_units(const USSERegType& reg_type, int *num, bool double_registers) {
    if (double_registers) {
        if (reg_type != REGTYPE_IMMEDIATE && reg_type != REGTYPE_SPECIAL)
            *num <<= 1;
    }
}

// This is for secondary programs at the start of execution, primary programs don't have to change it back to pa, o, etc.
void compute_register_final_pass(USSERegType& reg_bank) {
    if (secondary_program_state &&
        reg_bank != REGTYPE_FPINTERNAL && reg_bank != REGTYPE_FPCONSTANT && reg_bank != REGTYPE_IMMEDIATE)
        reg_bank = REGTYPE_SECATTR;
}

void perform_special_register_renaming(USSERegType& reg_bank, int *num) {
    int regnum = *num;

    if (reg_bank == REGTYPE_SPECIAL) {
        if (regnum & 0x40) {
            regnum &= ~0x40;
            reg_bank = REGTYPE_GLOBAL;
        } else
            reg_bank = REGTYPE_FPCONSTANT;
    }
}

void perform_temp_register_renaming(bool double_registers, USSERegType& reg_bank, int *num, int num_field_length, int index_reg_num = 0) {
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
        if (double_registers)
            _num_value >>= 1;
    }

    *num = _num_value;
}

USSERegType decode_src0_bank(uint32_t upper_instruction, bool allow_extended, int num, int num_field_length, uint32_t uext) {
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

void decode_src0_with_num(bool double_registers, uint32_t upper_instruction, bool allow_extended, int *num, int num_field_length, uint32_t uext) {
    USSERegType reg;

    reg = decode_src0_bank(upper_instruction, allow_extended, *num, num_field_length, uext);
    adjust_register_number_units(reg, num, double_registers);
    perform_special_register_renaming(reg, num);
    perform_temp_register_renaming(true, reg, num, num_field_length, 0);
    compute_register_final_pass(reg);

    printf("%s%d", get_register_operand_name(reg), *num);
}

USSERegType decode_src12_bank(uint32_t upper_instruction, uint32_t lower_instruction, bool allow_extended, int src_num, int num_field_length, uint32_t uext) {
    int bank_nr;
    bool extended_bank;
    
    static USSERegType register_type[4] = { REGTYPE_TEMP, REGTYPE_OUTPUT, REGTYPE_PRIMATTR, REGTYPE_SECATTR };
    static USSERegType extended_register_type[4] = { REGTYPE_INDEXED_LOW, REGTYPE_SPECIAL, REGTYPE_IMMEDIATE, REGTYPE_INDEXED_HIGH };

    switch (src_num) {
    default:
        [[fallthrough]];
    case 1:
        bank_nr = (lower_instruction & ~0x3FFFFFFFU) >> 30;
        break;
    case 2:
        bank_nr = (lower_instruction & ~0xCFFFFFFFU) >> 28;
        break;
    }

    if (allow_extended && (upper_instruction & uext) == uext)
        extended_bank = true;
    else
        extended_bank = false;

    return extended_bank ? extended_register_type[bank_nr] : register_type[bank_nr];
}

void decode_src12_with_num(bool double_registers, uint32_t upper_instruction, uint32_t lower_instruction, int src_num, bool allow_extended, int *num, int num_field_length, uint32_t uext) {
    USSERegType reg;

    reg = decode_src12_bank(upper_instruction, lower_instruction, true, src_num, num_field_length, uext);

    adjust_register_number_units(reg, num, double_registers);
    perform_special_register_renaming(reg, num);
    perform_temp_register_renaming(true, reg, num, num_field_length, 0);
    compute_register_final_pass(reg);

    printf("%s%d", get_register_operand_name(reg), *num);
}

USSERegType decode_dest_bank(int bank_nr, bool extended_bank) {
    static USSERegType register_type[4] = { REGTYPE_TEMP, REGTYPE_OUTPUT, REGTYPE_PRIMATTR, REGTYPE_INDEXED_LOW };
    static USSERegType extended_register_type[4] = { REGTYPE_SECATTR, REGTYPE_SPECIAL, REGTYPE_INDEX, REGTYPE_INDEXED_HIGH };

    if (bank_nr >= 4)
        return REGTYPE_UNDEF;

    return extended_bank ? extended_register_type[bank_nr] : register_type[bank_nr];
}

void decode_dest_with_num(bool double_registers, uint32_t upper_instruction, bool allow_extended, int *num, int num_field_length) {
    int bank_nr;
    bool extended_bank;
    USSERegType reg;

    bank_nr = (upper_instruction & ~0xFFFFFFFCU);
    if (allow_extended && (upper_instruction & 0x00080000) == 0x00080000) {
        extended_bank = true;
    } else {
        extended_bank = false;
    }

    reg = decode_dest_bank(bank_nr, extended_bank);
    adjust_register_number_units(reg, num, double_registers);
    perform_special_register_renaming(reg, num);
    perform_temp_register_renaming(true, reg, num, num_field_length, 0);
    compute_register_final_pass(reg);

    printf("%s%d", get_register_operand_name(reg), *num);
}
/*
    src0.index = 0;
    src1.index = 1;
    src2.index = 2;
    dest.index = 3;
*/

int main(int argc, char *argv[]) {
    reset_repeat_increase();
    reset_repeat_multiplier();
    uint64_t instructions[] = {
        0x00a2818661004306ull, // vmad
        0x00a2818661405307ull, // vmad
        0xf804014000000000ull, // nop
        0xfa44070000000000ull, // phase
        0xfa10000201010e01ull, // smlsi
        0x3880252183080080ull, // vmov
        0xfa14000001010101ull, // smlsi
        0x38800d2083f00000ull, // vmov
        0x40c00dbcffb9840aull, // pck
        0x18b18f80cf451102ull, // vmad
        0x18b18181c0011100ull, // vmad
        0x18b18181c042d101ull, // vmad
        0xfb275000a0200000ull, // spec
    };

    uint64_t instructions2[] = {
        0x00a2818661004306,//: VMAD2 sa8.xy sa8.xy c12.xx sa12.xy
        0x00a2818661405307,//: VMAD2 sa10.xy sa10.xy c12.xx sa14.xy
        0xf804014000000000,//: NOP
        0xfa44070000000000,//: PHAS
        0xfa10000201010e01,//: SMLSI  inc.1  inc.1 swizz.(2300)  inc.1 
        0x3880252183080080,//: VMOV.f32 o4.xy pa8.xy 
        0xfa14000001010101,//: SMLSI  inc.1  inc.1  inc.1  inc.1 
        0x38800d2083f00000,//: VMOV.f32 i0.xy pa0.xy 
        0x40c00dbcffb9840a,//: VPCKF32F32 i1.xyzw (sa8.xyzw sa10.xyzw) [noscale]
        0x18b18f80cf451102,//: VMAD i1.xyzw i0.yyyy sa4.xyzw i1.xyzw
        0x18b18181c0011100,//: VMAD o0.xy i0.xx sa0.xy i1.xy
        0x18b18181c042d101,//: VMAD o2.xy i0.xx sa2.xy i1.zw
        0xfb275000a0200000,//: SPEC category: 2, special: false
    };

    bool bad_opcode = false;

    secondary_program_state = true;
    int count = 0;
    for (auto& instruction : instructions2) {
        int common_opcode;
        
        if (bad_opcode)
            break;

        int upper_part = (int) (instruction >> 32);
        int lower_part = (int) instruction;

        common_opcode = ((instruction >> 32) & ~0x07FFFFFF) >> 27;

        if (++count > 3)
            secondary_program_state = false;
        switch (common_opcode) {
        case 0: // vecmad/vmad
        {
            bool is_f16 = (upper_part & 0x04000000) == 0x04000000; // vf16mad
            int dest_num, src0_num, src1_num, src2_num;

            printf("vec4mad ");
            dest_num = (lower_part & ~0xF03FFFFFU) >> 22;
            decode_dest_with_num(true, upper_part, true, &dest_num, 7);

            printf(".xyzw, "); // write mask

            src0_num = (lower_part & ~0xFFFC0FFFU) >> 12;
            decode_src0_with_num(true, upper_part, false, &src0_num, 7, 0);

            if (upper_part & 0x40000) {
                printf("%s", get_source_modifier_name(2));
            }

            int src0_swizzle = ((lower_part & ~0xFFF3FFFFU) >> 18) | ((upper_part & ~0xFFDFFFFFU) >> 21) << 2;
            printf(".%s", get_vecmad_source_swizzle(0, src0_swizzle));
            printf(", ");

            src1_num = (lower_part & ~0xFFFFF03FU) >> 6;
            decode_src12_with_num(true, upper_part, lower_part, 1, true, &src1_num, 7, 0x00020000);

            int src1_swizzle = ((lower_part & ~0xFFCFFFFFU) >> 20) | ((upper_part & ~0xFFFFEFFFU) >> 12) << 2;
            printf(".%s", get_vecmad_source_swizzle(1, src1_swizzle));
            printf(", ");

            src2_num = (lower_part & ~0xFFFFFFC0U);
            decode_src12_with_num(true, upper_part, lower_part, 2, true, &src2_num, 7, 0x00010000);

            int src2_swizzle = ((upper_part & ~0xFFFF1FFFU) >> 13);
            printf(".%s", get_vecmad_source_swizzle(2, src2_swizzle));

            printf(" // (two writes of components (xy) apply here based on instructions)\n");
            //bad_opcode = true;
            break;
        }
        case 3: // svec
        {
            bad_opcode = true;
            break;
        }
        case 7: // vecmov
        {
            int move_type = (upper_part & ~0xFFFF3FFFU) >> 14;
            int data_type;
            bool double_registers;
            int number_field_length;
            int repeat_count;

            data_type = (upper_part & ~0xFFFFF8FFU) >> 8;
            switch (data_type) {
            case DATA_TYPE_INT8:
                break;
            case DATA_TYPE_INT16:
                break;
            case DATA_TYPE_INT32:
                break;
            case DATA_TYPE_C10:
                break;
            case DATA_TYPE_F16:
                break;
            case DATA_TYPE_F32:
                break;
            default:
                printf("Invalid move data type\n");
                bad_opcode = true;
                continue;
            }

            if (data_type == DATA_TYPE_C10 || data_type == DATA_TYPE_F16 || data_type == DATA_TYPE_F32) {
                double_registers = true;
                number_field_length = 7;
            } else {
                double_registers = false;
                number_field_length = 6;
            }

            printf("vmov.f32 ");

            int dest_num = (lower_part & ~0xFF03FFFFU) >> 18;
            decode_dest_with_num(double_registers, upper_part, true, &dest_num, 7);
            printf(".xyzw, ");

            int src1_num = (lower_part & ~0xFFFFF03FU) >> 6;

            decode_src12_with_num(true, upper_part, lower_part, 1, true, &src1_num, number_field_length, 0x00020000U);
            printf(".xyzw\n");

            // TODO implement repeats
            repeat_count = ((upper_part & ~0xFFFFCFFFU) >> 12) + 1;

            for (int i = 0; i < repeat_count; i++) {
                //int dest_repeat_offset = get_repeat_offset(USSERegType::REGTYPE_FPCONSTANT, i, 3);
                int src1_repeat_offset = get_repeat_offset(USSERegType::REGTYPE_FPCONSTANT, i, 1);
                printf("%d\n", src1_num + (src1_repeat_offset << 1));
            }
            printf("\n");
            // bad_opcode = true;
            break;
        }
        case 8: // vecpck
        {
            static const USSEOpcodeType fmt_ops[][8] =
            {
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

            uint32_t dest_mask = (upper_part & ~0xFFFFFFC3U) >> 2;
            uint32_t dest_fmt = (upper_part & ~0xFFFFFE3FU) >> 6;
            uint32_t src_fmt = (upper_part & ~0xFFFFF1FFU) >> 9;
            printf("vpck.f32f32 ");

            int repeat_count = (upper_part & ~0xFFFF0FFFU) >> 12;
            uint32_t channel;
            uint32_t swizzle_src;
            bool replicate_conversion;
            bool src_float, dest_float;
            bool sparse_swizzle;
            uint32_t swizzle_sel[4];

            USSEOpcodeType opcode = fmt_ops[dest_fmt][src_fmt];

            int dest_num = (lower_part & ~0xF01FFFFFU) >> 21;
            decode_dest_with_num(false, upper_part, true, &dest_num, 7);
            printf(".xyzw, ");

            swizzle_src = 2;

            if (src_fmt == 6 || src_fmt == 5 || src_fmt == 7) {
                int src1_num = (lower_part & ~0xFFFFC0FFU) >> 8;
                printf("(");
                decode_src12_with_num(true, upper_part, lower_part, 1, true, &src1_num, 7, 0x00020000);
                printf(".xyzw");
            
                if (src_fmt == 6) {
                    int src2_num = (lower_part & ~0xFFFFFF81U) >> 1;
                    printf(", ");
                    decode_src12_with_num(true, upper_part, lower_part, 1, true, &src2_num, 7, 0x00020000);
                    printf(".xyzw");
                }
                printf(")");
            }

            // TODO implement repeated instructions
            printf("\n");
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
                
                printf("spec (moectrl)\n");
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
    return 0;
}