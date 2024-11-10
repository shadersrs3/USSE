#ifndef _BETAVITA_SHADER_RECOMPILER_H
#define _BETAVITA_SHADER_RECOMPILER_H

#include <cstdint>

#include <vector>
#include <shader_processor.h>

namespace betavita::usse {
struct USSEInstruction;

typedef std::vector<USSEInstruction> USSEInstructionList;

struct Recompiler {
private:
    enum : uint32_t {
        PACK2XF16_RELOCATION = (1 << 0)
    };

    const USSEInstructionList *usse_instruction_list;
    ShaderProcessor processor;
    uint32_t function_relocation;
public:
    Recompiler();
    void reset_recompiler();
    void set_instruction_list(const USSEInstructionList *instruction_list);
    void add_pack2xF16();
    void recompile();
    const char *get_source();
};
}

#endif /* _BETAVITA_SHADER_RECOMPILER_H */