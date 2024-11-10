#include <cstdio>
#include <cstdint>
#include <cstdlib>

#include <string>
#include <algorithm>
#include <vector>

#include <shader_processor.h>
#include <shader_types.h>
#include <shader_decoder.h>

int main(int argc, char *argv[]) {
    using namespace betavita::usse;

    uint64_t instructions[] = {
        0x40800d7ef0198002
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

    reset_decoder();
    set_instruction_stream(instructions2, sizeof instructions2 / sizeof *instructions2);
    decode_instruction_stream();

    ShaderProcessor processor;
    processor.set_shader_version("430");
    processor.set_function_target("null");

    processor.add_variable("vec4", "o[20]");
    processor.add_variable("vec4", "pa[32]");
    processor.add_variable("vec4", "sa[32]");
    processor.add_variable("vec4", "i[4]");
    processor.add_line("");
    processor.create_function("main");
    processor.set_function_target("main");

    static bool defined_pack_function;
    defined_pack_function = false;
    static int temp_reg;

    temp_reg = 0;
    for (auto& i : get_instruction_list()) {
        switch (i.op) {
        case OP_VMOV: // vmov
        {
            std::string x;
            int write_mask;
            std::string write_mask_str;
            std::string swizzle;
            int reg_dest, reg_src1;
            std::string final_output_register;

            if (i.registers[0].type != REGTYPE_FPINTERNAL) {
                reg_dest = i.registers[0].num >> 2;
                write_mask = (i.write_mask << (i.registers[0].num % 4)) & 0xF;
            } else {
                reg_dest = i.registers[0].num;
                write_mask = i.write_mask;
            }

            x += std::string(get_register_operand_name(i.registers[0].type)) + ("[" + std::to_string(reg_dest) + "].") + get_write_mask_components(write_mask) + " = ";

            if (!(i.registers[1].type == REGTYPE_FPINTERNAL || i.registers[1].type == REGTYPE_FPCONSTANT)) {
                reg_src1 = i.registers[1].num >> 2;
                write_mask = (i.write_mask << (i.registers[1].num % 4)) & 0xF;
                swizzle = get_swizzle_string_with_write_mask(i.registers[1].swizzle_index, write_mask);
                x += get_register_operand_name(i.registers[1].type) +  ("[" + std::to_string(reg_src1) + "].") + get_write_mask_components(write_mask) + ";";
            } else {
                reg_src1 = i.registers[1].num;
                write_mask = i.write_mask;
                swizzle = get_swizzle_string_with_write_mask(i.registers[1].swizzle_index, write_mask);

                if (i.registers[1].type == REGTYPE_FPCONSTANT) {
                    if (i.registers[1].num == 1 && get_write_mask_components_count(write_mask) == 1)
                        x += "1.0;";
                } else {
                    x += get_register_operand_name(i.registers[1].type) +  ("[" + std::to_string(reg_src1) + "].") + get_write_mask_components(write_mask) + ";";
                }
            }

            processor.add_line(x);
            break;
        }
        case OP_V4MAD: // v4mad
        {
            std::string x;
            std::string write_mask_str;
            int reg_dest, reg_src0, reg_src1, reg_src2;
            int write_mask;
            std::string swizzle;

            (void) reg_src1;

            if (i.registers[0].type != REGTYPE_FPINTERNAL) {
                reg_dest = i.registers[0].num >> 2;
                write_mask = (i.write_mask << (i.registers[0].num % 4)) & 0xF;
            } else {
                reg_dest = i.registers[0].num;
                write_mask = i.write_mask;
            }

            x += std::string(get_register_operand_name(i.registers[0].type)) + ("[" + std::to_string(reg_dest) + "].") + get_write_mask_components(write_mask) + " = ";
            x += "(";

            if (!(i.registers[1].type == REGTYPE_FPINTERNAL || i.registers[1].type == REGTYPE_FPCONSTANT)) {
                reg_src0 = i.registers[1].num >> 2;
                write_mask = (i.write_mask << (i.registers[1].num % 4)) & 0xF;
                swizzle = get_swizzle_string_with_write_mask(i.registers[1].swizzle_index, write_mask);
            } else {
                reg_src0 = i.registers[1].num;
                write_mask = i.write_mask;
                swizzle = get_swizzle_string_with_write_mask(i.registers[1].swizzle_index, write_mask);
            }

            x += std::string(get_register_operand_name(i.registers[1].type)) +  ("[" + std::to_string(reg_src0) + "].") + swizzle;

            x += " * ";

            if (!(i.registers[2].type == REGTYPE_FPINTERNAL || i.registers[2].type == REGTYPE_FPCONSTANT)) {
                reg_src1 = i.registers[2].num >> 2;
                write_mask = (i.write_mask << (i.registers[2].num % 4)) & 0xF;
                swizzle = get_swizzle_string_with_write_mask(i.registers[2].swizzle_index, write_mask);
            } else {
                reg_src1 = i.registers[1].num;
                write_mask = i.write_mask;
                swizzle = get_swizzle_string_with_write_mask(i.registers[2].swizzle_index, write_mask);
            }

            if (i.registers[2].type == REGTYPE_FPCONSTANT) {
                int num_comps = get_write_mask_components_count(write_mask);

                if (i.registers[2].num == 12 && num_comps == 2)
                    x += "vec2(0.5)";
                else {
                    printf("Bad v4mad fpconstant\n");
                    std::exit(0);
                }
            } else {
                printf("bad v4mad\n");
                std::exit(0);
            }

            x += ") + ";

            if (!(i.registers[3].type == REGTYPE_FPINTERNAL || i.registers[3].type == REGTYPE_FPCONSTANT)) {
                reg_src2 = i.registers[3].num >> 2;
                write_mask = (i.write_mask << (i.registers[3].num % 4)) & 0xF;
                swizzle = get_swizzle_string_with_write_mask(i.registers[3].swizzle_index, write_mask);
            } else {
                reg_src2 = i.registers[3].num;
                write_mask = i.write_mask;
                swizzle = get_swizzle_string_with_write_mask(i.registers[3].swizzle_index, write_mask);
            }

            x += std::string(get_register_operand_name(i.registers[1].type)) +  ("[" + std::to_string(reg_src2) + "].") + swizzle;

            x += ";";
            processor.add_line(x);
            break;
        }
        case OP_VMUL:
        {
            std::string x;
            int write_mask;
            std::string write_mask_str;
            std::string swizzle;
            int reg_dest, reg_src1, reg_src2;

            if (i.registers[0].type != REGTYPE_FPINTERNAL) {
                reg_dest = i.registers[0].num >> 2;
                write_mask = (i.write_mask << (i.registers[0].num % 4)) & 0xF;
            } else {
                reg_dest = i.registers[0].num;
                write_mask = i.write_mask;
            }

            x += std::string(get_register_operand_name(i.registers[0].type)) + ("[" + std::to_string(reg_dest) + "].") + get_write_mask_components(write_mask) + " = ";

            if (i.registers[1].type == REGTYPE_IMMEDIATE) {
                reg_src1 = i.registers[1].num;
                write_mask = i.write_mask;
                swizzle = get_swizzle_string_with_write_mask(i.registers[1].swizzle_index, write_mask);
            
                x += "vec2(";
                x.push_back(swizzle[0]);
                x += ".0, ";
                x.push_back(swizzle[1]);
                x += ".0)";
            } else if (!(i.registers[1].type == REGTYPE_FPINTERNAL || i.registers[1].type == REGTYPE_FPCONSTANT)) {
                reg_src1 = i.registers[1].num >> 2;
                write_mask = (i.write_mask << (i.registers[1].num % 4)) & 0xF;
                swizzle = get_swizzle_string_with_write_mask(i.registers[1].swizzle_index, write_mask);
                x += std::string(get_register_operand_name(i.registers[1].type)) +  ("[" + std::to_string(reg_src1) + "].") + get_write_mask_components(write_mask);
            } else {
                reg_src1 = i.registers[1].num;
                write_mask = i.write_mask;
                swizzle = get_swizzle_string_with_write_mask(i.registers[1].swizzle_index, write_mask);
                x += std::string(get_register_operand_name(i.registers[1].type)) +  ("[" + std::to_string(reg_src1) + "].") + get_write_mask_components(write_mask);
            }

            if (!(i.registers[2].type == REGTYPE_FPINTERNAL || i.registers[2].type == REGTYPE_FPCONSTANT)) {
                reg_src2 = i.registers[2].num >> 2;
                write_mask = (i.write_mask << (i.registers[2].num % 4)) & 0xF;
                swizzle = get_swizzle_string_with_write_mask(i.registers[2].swizzle_index, write_mask);
            } else {
                reg_src2 = i.registers[2].num;
                write_mask = i.write_mask;
                swizzle = get_swizzle_string_with_write_mask(i.registers[2].swizzle_index, write_mask);
            }

            x += " * ";

            if (i.registers[2].type == REGTYPE_FPCONSTANT) {
                if (reg_src2 == 1) {
                    x += "vec2(1.0)." + swizzle;
                } else {
                    printf("unimplemented fpconstant %d\n", reg_src2);
                    std::exit(0);
                }
            } else {
                x += std::string(get_register_operand_name(i.registers[2].type)) +  ("[" + std::to_string(reg_src2) + "].") + get_write_mask_components(write_mask);
            }
            x += ";";

            processor.add_line(x);
            break;
        }
        case OP_VF16MUL:
        {
            std::string x;
            int write_mask;
            std::string write_mask_str;
            std::string swizzle;
            int reg_dest, reg_src1, reg_src2;

            if (!defined_pack_function) {
                processor.create_function("pack2xF16");
                processor.set_function_target("pack2xF16");
                processor.set_function_parameters("vec2 x");
                processor.set_function_return_type("float");
                processor.add_line("return uintBitsToFloat(packHalf2x16(x));");
                processor.finalize_function("pack2xF16");
                processor.set_function_target("main");
                defined_pack_function = true;
            }

            if (i.registers[0].type != REGTYPE_FPINTERNAL) {
                reg_dest = i.registers[0].num >> 2;
                write_mask = (i.write_mask << (i.registers[0].num % 4)) & 0xF;
            } else {
                reg_dest = i.registers[0].num;
                write_mask = i.write_mask;
            }


            x += "vec4 temp" + std::to_string(temp_reg) + " = ";

            if (!(i.registers[1].type == REGTYPE_FPINTERNAL || i.registers[1].type == REGTYPE_FPCONSTANT)) {
                reg_src1 = i.registers[1].num >> 2;
                write_mask = (i.write_mask << (i.registers[1].num % 4)) & 0xF;
                swizzle = get_swizzle_string_with_write_mask(i.registers[1].swizzle_index, write_mask);
                x += std::string(get_register_operand_name(i.registers[1].type)) +  ("[" + std::to_string(reg_src1) + "].") + get_write_mask_components(write_mask);
            } else {
                reg_src1 = i.registers[1].num;
                write_mask = i.write_mask;
                swizzle = get_swizzle_string_with_write_mask(i.registers[1].swizzle_index, write_mask);
                x += std::string(get_register_operand_name(i.registers[1].type)) +  ("[" + std::to_string(reg_src1) + "].") + get_write_mask_components(write_mask);
            }

            if (!(i.registers[2].type == REGTYPE_FPINTERNAL || i.registers[2].type == REGTYPE_FPCONSTANT)) {
                reg_src2 = i.registers[2].num >> 2;
                write_mask = (i.write_mask << (i.registers[2].num % 4)) & 0xF;
                swizzle = get_swizzle_string_with_write_mask(i.registers[2].swizzle_index, write_mask);
            } else {
                reg_src2 = i.registers[2].num;
                write_mask = i.write_mask;
                swizzle = get_swizzle_string_with_write_mask(i.registers[2].swizzle_index, write_mask);
            }

            x += " * ";

            if (i.registers[2].type == REGTYPE_FPCONSTANT) {
                if (reg_src2 == 1) {
                    x += "vec2(1.0)." + swizzle;
                } else {
                    printf("unimplemented fpconstant %d\n", reg_src2);
                    std::exit(0);
                }
            } else {
                x += std::string(get_register_operand_name(i.registers[2].type)) +  ("[" + std::to_string(reg_src2) + "].") + get_write_mask_components(write_mask);
            }
            x += ";\n";

            x += "    " + std::string(get_register_operand_name(i.registers[0].type)) + ("[" + std::to_string(reg_dest) + "].") + get_write_mask_components(write_mask & 3) + " = ";
            x += ("vec2(pack2xF16(temp" + std::to_string(temp_reg) + ".xy), ") + "pack2xF16(temp" + std::to_string(temp_reg) + ".zw)" ");";
            temp_reg++;

            processor.add_line(x);
            break;
        }
        case OP_VPCKF16F32: // vpck.f16f32
        {

            break;
        }
        case OP_VPCKF32F32: // vpck.f32f32
        {
            std::string x;
            int write_mask;
            std::string write_mask_str;
            std::string swizzle;
            int reg_dest, reg_src1, reg_src2;

            if (i.registers[0].type != REGTYPE_FPINTERNAL) {
                reg_dest = i.registers[0].num >> 2;
                write_mask = (i.write_mask << (i.registers[0].num % 4)) & 0xF;
            } else {
                reg_dest = i.registers[0].num;
                write_mask = i.write_mask;
            }

            x += std::string(get_register_operand_name(i.registers[0].type)) + ("[" + std::to_string(reg_dest) + "].") + get_write_mask_components(write_mask) + " = ";

            if (!(i.registers[1].type == REGTYPE_FPINTERNAL || i.registers[1].type == REGTYPE_FPCONSTANT)) {
                reg_src1 = i.registers[1].num >> 2;
                write_mask = (i.write_mask << (i.registers[1].num % 4)) & 0x3;
                swizzle = get_swizzle_string_with_write_mask(i.registers[1].swizzle_index, write_mask);
            } else {
                reg_src1 = i.registers[1].num;
                write_mask = i.write_mask;
                swizzle = get_swizzle_string_with_write_mask(i.registers[1].swizzle_index, write_mask);
            }

            x += "vec4(";

            x += std::string(get_register_operand_name(i.registers[1].type)) +  ("[" + std::to_string(reg_src1) + "].") + get_write_mask_components(write_mask);

            if (!(i.registers[2].type == REGTYPE_FPINTERNAL || i.registers[2].type == REGTYPE_FPCONSTANT)) {
                reg_src2 = i.registers[2].num >> 2;
                write_mask = (i.write_mask << (i.registers[2].num % 4)) & 0xF;
                swizzle = get_swizzle_string_with_write_mask(i.registers[2].swizzle_index, write_mask);
            } else {
                reg_src2 = i.registers[2].num;
                write_mask = i.write_mask;
                swizzle = get_swizzle_string_with_write_mask(i.registers[2].swizzle_index, write_mask);
            }

            x += ", ";

            x += std::string(get_register_operand_name(i.registers[2].type)) +  ("[" + std::to_string(reg_src2) + "].") + get_write_mask_components(write_mask);
            x += ");";

            processor.add_line(x);
            break;
        }
        case OP_VMAD4: // vmad4
        {
            std::string x;
            std::string write_mask_str;
            int reg_dest, reg_src0, reg_src1, reg_src2;
            int write_mask;
            std::string swizzle;

            (void) reg_src1;

            if (i.registers[0].type != REGTYPE_FPINTERNAL) {
                reg_dest = i.registers[0].num >> 2;
                write_mask = (i.write_mask << (i.registers[0].num % 4)) & 0xF;
            } else {
                reg_dest = i.registers[0].num;
                write_mask = i.write_mask;
            }

            x += std::string(get_register_operand_name(i.registers[0].type)) + ("[" + std::to_string(reg_dest) + "].") + get_write_mask_components(write_mask) + " = ";
            x += "(";

            if (!(i.registers[1].type == REGTYPE_FPINTERNAL || i.registers[1].type == REGTYPE_FPCONSTANT)) {
                reg_src0 = i.registers[1].num >> 2;
                write_mask = (i.write_mask << (i.registers[1].num % 4)) & 0xF;
                swizzle = get_swizzle_string_with_write_mask(i.registers[1].swizzle_index, write_mask);
            } else {
                reg_src0 = i.registers[1].num;
                write_mask = i.write_mask;
                swizzle = get_swizzle_string_with_write_mask(i.registers[1].swizzle_index, write_mask);
            }

            x += std::string(get_register_operand_name(i.registers[1].type)) +  ("[" + std::to_string(reg_src0) + "].") + swizzle;

            x += " * ";

            if (!(i.registers[2].type == REGTYPE_FPINTERNAL || i.registers[2].type == REGTYPE_FPCONSTANT)) {
                reg_src1 = i.registers[2].num >> 2;
                write_mask = (i.write_mask << (i.registers[2].num % 4)) & 0xF;
                swizzle = get_swizzle_string_with_write_mask(i.registers[2].swizzle_index, write_mask);
            } else {
                reg_src1 = i.registers[1].num;
                write_mask = i.write_mask;
                swizzle = get_swizzle_string_with_write_mask(i.registers[2].swizzle_index, write_mask);
            }

            if (i.registers[2].type == REGTYPE_FPCONSTANT) {
                int num_comps = get_write_mask_components_count(write_mask);

                if (i.registers[2].num == 12 && num_comps == 2)
                    x += "vec2(0.5)";
                else {
                    printf("Bad v4mad fpconstant\n");
                    std::exit(0);
                }
            } else {
                x += std::string(get_register_operand_name(i.registers[2].type)) +  ("[" + std::to_string(reg_src1) + "].") + swizzle;
            }

            x += ") + ";

            if (!(i.registers[3].type == REGTYPE_FPINTERNAL || i.registers[3].type == REGTYPE_FPCONSTANT)) {
                reg_src2 = i.registers[3].num >> 2;
                write_mask = (i.write_mask << (i.registers[3].num % 4)) & 0xF;
                swizzle = get_swizzle_string_with_write_mask(i.registers[3].swizzle_index, write_mask);
            } else {
                reg_src2 = i.registers[3].num;
                write_mask = i.write_mask;
                swizzle = get_swizzle_string_with_write_mask(i.registers[3].swizzle_index, write_mask);
            }

            x += std::string(get_register_operand_name(i.registers[1].type)) +  ("[" + std::to_string(reg_src2) + "].") + swizzle;

            x += ";";
            processor.add_line(x);
            break;
        }
        default:
            printf("unimplemented op %d\n", i.op);
        }
    }

    processor.finalize_function("main");
    printf("%s\n", processor.get_shader_code());
    return 0;
}