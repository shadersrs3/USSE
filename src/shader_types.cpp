#include <shader_types.h>

namespace betavita::usse {
const char *get_register_type_name(const USSERegType& reg) {
    static const char *reg_name[] = {
        "temp", "primattr", "output", "secattr",
        "fpinternal", "special", "global", "fpconstant",
        "immediate", "index", "indexed", "indexedlow", "indexedhigh"
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

int get_write_mask_components_count(int write_mask) {
    int count = 0;
    for (int i = 0; i < 4; i++) {
        if ((write_mask & (1 << i)) != 0) {
            count++;
        }
    }
    return count;
}

std::string get_write_mask_components(int write_mask) {
    std::string p;
    static const char mask[] = { 'x', 'y', 'z', 'w' };
    for (int i = 0; i < 4; i++) {
        if ((write_mask & (1 << i)) != 0) {
            p += mask[i];
        }
    }
    return p;
}

std::string get_swizzle_string(uint32_t idx) {
    std::string x;

    const char *str[] = { "x", "y", "z", "w", "0", "1", "2", "half" };
    for (int i = 0; i < 4; i++)
        x += str[(idx >> (3 * i)) & 7];
    return x;
}

std::string get_swizzle_string_with_write_mask(uint32_t idx, int write_mask) {
    std::string in = get_swizzle_string(idx);
    std::string out;

    for (int i = 0; i < 4; i++) {
        if ((write_mask & (1 << i)) != 0) {
            out += in[i];
        }
    }
    return out;
}
}