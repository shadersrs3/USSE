#ifndef _BETAVITA_SHADER_OPCODE_LIST_H
#define _BETAVITA_SHADER_OPCODE_LIST_H

#include <cstdint>

#include <vector>
#include <string>

namespace betavita::usse {
enum USSEOpcodeType : int {
    // vecmov ops (lower 6 bits are reserved for move data type flags on USSEInstruction)
    OP_VMOV,
    OP_VMOVC,
    OP_VMOVCU8,

    // vecmad ops
    OP_VF16MAD,
    OP_V4MAD,

    // vecnmad ops (f32)

    OP_VMUL,
    OP_VADD,
    OP_VFRC,
    OP_VDSX,
    OP_VDSY,
    OP_VMIN,
    OP_VMAX,
    OP_VDP,

    // vecnmad ops (f16)

    OP_VF16MUL,
    OP_VF16ADD,
    OP_VF16FRC,
    OP_VF16DSX,
    OP_VF16DSY,
    OP_VF16MIN,
    OP_VF16MAX,
    OP_VF16DP,

    // pck ops
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

    // moectrl ops
    OP_SMOA,
    OP_SMR,
    OP_SMLSI,
    OP_SMBO,
    OP_IMO,
    OP_SETFC,

    // single issue ops
    OP_VDP3,
    OP_VDP4,
    OP_VMAD3,
    OP_VMAD4,

    OP_UNDEF,
};

enum USSEMoveTypeFlags : uint32_t {
    MOVE_TYPE_INT8 = (1 << 0),
    MOVE_TYPE_INT16 = (1 << 1),
    MOVE_TYPE_INT32 = (1 << 2),
    MOVE_TYPE_C10 = (1 << 3),
    MOVE_TYPE_F16 = (1 << 4),
    MOVE_TYPE_F32 = (1 << 5)
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
    REGTYPE_INDEXED,
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

enum USSESourceModifier : int {
    SOURCE_MODIFIER_NONE,
    SOURCE_MODIFIER_NEG,
    SOURCE_MODIFIER_ABS,
    SOURCE_MODIFIER_NEGABS,
};

enum IncrementModeType : int {
    INCREMENT_MODE_US = (1 << 0),
    INCREMENT_MODE_GPI = (1 << 1),
    INCREMENT_MODE_BOTH = (1 << 2),
    INCREMENT_MODE_MOE = (1 << 3)
};

struct SGX543Register {
    USSERegType type;
    int num;
    int swizzle_index;
    int modifier;
    uint32_t flags;
};

struct USSEInstruction {
    USSEOpcodeType op;
    uint32_t flags;
    int write_mask;
    std::vector<SGX543Register> registers;
};

typedef std::vector<USSEInstruction> USSEInstructionList;

const char *get_register_type_name(const USSERegType& reg);
const char *get_register_operand_name(const USSERegType& reg);
const char *get_source_modifier_name(int index);
int get_write_mask_components_count(int write_mask);
std::string get_write_mask_components(int write_mask);
std::string get_swizzle_string(uint32_t idx);
std::string get_swizzle_string_with_write_mask(uint32_t idx, int write_mask);
}

#endif