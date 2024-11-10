#ifndef _BETAVITA_SHADER_DECODER_H
#define _BETAVITA_SHADER_DECODER_H

#include <cstdint>

#include <shader_types.h>

namespace betavita::usse {
void set_secondary_program_state(bool state);
void reset_decoder();
void set_instruction_stream(const uint64_t *instructions, int instruction_length);
void decode_instruction_stream();
USSEInstructionList get_instruction_list();
}

#endif /* _BETAVITA_SHADER_DECODER_H */