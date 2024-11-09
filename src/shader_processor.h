#ifndef _BETAVITA_SHADER_PROCESSOR_H
#define _BETAVITA_SHADER_PROCESSOR_H

#include <vector>
#include <string>

struct ShaderBlock;
struct ShaderConditionBlock;

struct ShaderStatement {
    enum class Type { VariableInitAssign, VariableAssign };

    Type type;

    virtual ~ShaderStatement() {}
};

struct ShaderVariableInitAssignStatement : public ShaderStatement {
    int init_type;
    char name[32];
    char assign[128];

    ~ShaderVariableInitAssignStatement() {}
};

struct ShaderVariableAssignStatement : public ShaderStatement {
    char name[32];
    char assign[128];

    ~ShaderVariableAssignStatement() {}
};

struct ShaderIfElseStatement : public ShaderStatement {
    std::vector<ShaderConditionBlock *> if_block_list;
    ShaderBlock *else_block;

    ~ShaderIfElseStatement() {}
};

struct ShaderBlock {
    enum class Type { SequenceBlock, ConditionBlock, LoopBlock, BranchBlock };
    int type;
    std::vector<ShaderBlock *> block_list;

    virtual ~ShaderBlock() {}
};

struct ShaderConditionBlock : public ShaderBlock {
    char condition[128];

    ~ShaderConditionBlock() {}
};

struct ShaderSequenceBlock : public ShaderBlock {
    std::vector<ShaderStatement *> statement_list;

    ~ShaderSequenceBlock() {}
};

struct ShaderFunction {
    std::string name;
    std::vector<ShaderBlock *> block;
};

struct ShaderProcessor {
private:
    struct Function {
        std::string name;
        std::string function_parameters;
        std::string return_type;
        int current_scope_level;
        std::vector<std::string> statement;
    };

    std::vector<Function *> function_list;
    std::string current_function_target;
    std::string shader_code;
private:
    std::vector<std::string> *get_current_function_statement();
public:
    ShaderProcessor();
    ~ShaderProcessor();

    void set_shader_version(const std::string& version);
    void add_extension(const std::string& extension);
    void add_variable(const std::string& type, const std::string& name);
    void assign_variable(const std::string& name, const std::string& assignment);
    void add_and_assign_variable(const std::string& type, const std::string& name, const std::string& assignment);
    void add_line(const std::string& line);
    void set_function_return_type(const std::string& return_type);
    void set_function_parameters(const std::string& params);
    void create_function(const std::string& function_name);
    void set_function_target(const std::string& function_name);
    void finalize_function(const std::string& function_name);
    const char *get_shader_code();
};

#endif /* _BETAVITA_SHADER_PROCESSOR_H */