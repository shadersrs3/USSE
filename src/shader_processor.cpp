#include <cstdio>

#include "shader_processor.h"

ShaderProcessor::ShaderProcessor() {
}

ShaderProcessor::~ShaderProcessor() {
    for (auto& i : function_list)
        delete i;
}

void ShaderProcessor::set_shader_version(const std::string& version) {
    shader_code += "#version " + version + "\n";
}

void ShaderProcessor::add_extension(const std::string& extension) {

}

void ShaderProcessor::add_variable(const std::string& type, const std::string& name) {
    if (current_function_target == "null") {
        shader_code += type + " " + name + ";\n";
        return;
    }

    for (auto& i : function_list) {
        if (current_function_target == i->name) {
            std::string output;
            for (int x = 0; x < i->current_scope_level; x++) {
                output += "    ";
            }

            i->statement.push_back(output + type + " " + name + ";\n");
            return;
        }
    }
}

void ShaderProcessor::assign_variable(const std::string& name, const std::string& assignment) {
    for (auto& i : function_list) {
        if (current_function_target == i->name) {
            std::string output;
            for (int x = 0; x < i->current_scope_level; x++) {
                output += "    ";
            }

            i->statement.push_back(output + name + " = " + assignment + ";\n");
            return;
        }
    }
}

void ShaderProcessor::add_and_assign_variable(const std::string& type, const std::string& name, const std::string& assignment) {
}

void ShaderProcessor::add_line(const std::string& line) {
    if (current_function_target == "null") {
        shader_code += line + "\n";
        return;
    }

    for (auto& i : function_list) {
        if (current_function_target == i->name) {
            std::string output;
            for (int x = 0; x < i->current_scope_level; x++) {
                output += "    ";
            }

            i->statement.push_back(output + line + "\n");
            return;
        }
    }
}

void ShaderProcessor::set_function_return_type(const std::string& return_type) {
    for (auto& i : function_list) {
        if (i->name == current_function_target) {
            i->return_type = return_type;
            return;
        }
    }
}

void ShaderProcessor::set_function_parameters(const std::string& params) {
    for (auto& i : function_list) {
        if (i->name == current_function_target) {
            i->function_parameters = params;
            return;
        }
    }
}

void ShaderProcessor::create_function(const std::string& function_name) {
    Function *function;

    function = new Function;
    function->name = function_name;
    function->return_type = "void";
    function->current_scope_level = 1;
    function_list.push_back(function);
}

void ShaderProcessor::set_function_target(const std::string& function_name) {
    current_function_target = function_name;
}

void ShaderProcessor::finalize_function(const std::string& function_name) {
    for (auto& i : function_list) {
        if (i->name == function_name) {
            shader_code += i->return_type + " " + function_name + "(" + i->function_parameters + ") {\n";
            for (auto& x : i->statement) {
                shader_code += x;
            }
            shader_code += "}\n\n";
        }
    }
}

const char *ShaderProcessor::get_shader_code() {
    return shader_code.c_str();
}