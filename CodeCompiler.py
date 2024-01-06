import json
from ply import lex, yacc
from StatementParser import *
import subprocess
import os
import jpype


class MySymbolTable:
    def __init__(self):
        self.table = {}  # 符号表的主体，使用字典存储
        self.parent = None  # 父母级符号表，应对多级作用域
        self.brother = []  # 兄弟级符号表，应对函数作用域

    # 插入符号，返回True/False
    def insert(self, name, symbol_type, symbol_value):
        """插入符号到符号表"""
        if name not in self.table:
            self.table[name] = {'type': symbol_type, 'value': symbol_value}
            return True
        else:
            print(f"Error: Identifier '{name}' already declared in the same scope.")
            return False

    # 查找符号，返回Object/None
    def lookup(self, name):
        """查找符号在符号表中的信息"""
        if name in self.table:
            return self.table[name]
        else:
            temp_symbol_table = self.parent
            while temp_symbol_table is not None:
                if name in temp_symbol_table.table:
                    return temp_symbol_table.table[name]
                temp_symbol_table = temp_symbol_table.parent
            print(f"Error: Identifier '{name}' not found in the all scope.")
            self.display()
            return None

    # 删除符号，返回True/False，参数为0则全部清空
    def delete(self, name):
        if (name != 0) and (self.lookup(name) is None):
            return False
        else:
            temp_symbol_table = self
            while temp_symbol_table is not None:
                if name == 0:
                    temp_symbol_table.table.clear()
                elif name in temp_symbol_table.table:
                    temp_symbol_table.table.pop(name, None)
                    return True
                temp_symbol_table = temp_symbol_table.parent
            return True

    # 更新符号，返回True/False/None
    def update(self, name, symbol_type, symbol_value):
        """更新符号在符号表中的信息"""
        if name in self.table:
            if self.table[name]["type"] is not symbol_type: return False
            self.table[name] = {'type': symbol_type, 'value': symbol_value}
            return True
        else:
            temp_symbol_table = self.parent
            while temp_symbol_table is not None:
                if name in temp_symbol_table.table:
                    if temp_symbol_table.table[name]["type"] is not symbol_type: return False
                    temp_symbol_table.table[name] = {'type': symbol_type, 'value': symbol_value}
                    return True
                temp_symbol_table = temp_symbol_table.parent
            print(f"Error: Identifier '{name}' not found in the all scope.")
            return None

    # 打印符号，显示所有符号
    def display(self):
        """显示各级符号表内容"""
        print("Symbol Table:")
        temp = self
        while temp is not None:
            for name, info in temp.table.items():
                print(f"Name: {name}, Type: {info['type']}, Value: {info['value']}")
            temp = temp.parent

    # 字典转为JSON字符串
    def dict_to_json(self, my_dict):
        json_str = json.dumps(my_dict)
        return json_str

    # JSON字符串转为字典
    def json_to_dict(self, json_str):
        my_dict = json.loads(json_str)
        return my_dict


symbol_table = MySymbolTable()
state = False
# 定义词法分析器（Lex）
tokens = (
    'NUMBER',  # 算术 整数
    'FLOAT',  # 算术 实数
    'PLUS',  # 算术 加法
    'MINUS',  # 算术 减法
    'TIMES',  # 算术 乘法
    'DIVIDE',  # 算术 除法
    'MOD',  # 算术 求余
    'LPAREN',  # 算术 括号
    'RPAREN',  # 算术 括号
    'IDENTIFIER',  # 算术 变量
    'DEQUAL',  # 逻辑 等于 ==
    'NOTEQU',  # 逻辑 不等 !=
    'OR',  # 逻辑 或 ||
    'AND',  # 逻辑 与 &&
    'NOT',  # 逻辑 非 !!
    'MORETHAN',  # 逻辑 大于 >
    'LESSTHAN',  # 逻辑 小于 <
    'MORETHANEQ',  # 逻辑 大于等于 >=
    'LESSTHANEQ',  # 逻辑 小于等于 <=
    'LBRACK',  # 列表 方括号
    'RBRACK',  # 列表 方括号
    'COMMA',  # 列表 逗号
    'TEXT',  # 文本 单纯文本
)
# 注意，算术表达式的计算优先级高于逻辑表达式，表达式（算术表达式，逻辑表达式，文本表达式）
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_MOD = r'%'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_DEQUAL = r'=='
t_NOTEQU = r'!='
t_OR = r'\|\|'
t_AND = r'&&'
t_NOT = r'!!'
t_MORETHAN = r'>'
t_LESSTHAN = r'<'
t_MORETHANEQ = r'>='
t_LESSTHANEQ = r'<='
t_LBRACK = r'\['
t_RBRACK = r'\]'
t_COMMA = r','
t_ignore = ' \t'


def t_FLOAT(t):
    r'\d+(\.\d+)'
    t.value = float(t.value)
    return t


def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_IDENTIFIER(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = 'IDENTIFIER'
    return t


quotation = "\""
rule = "([^" + quotation + r"\\]|\\.)*"


@lex.TOKEN(quotation + rule + quotation)
def t_TEXT(t):
    t.type = 'TEXT'
    return t


def t_error(t):
    print(f"Unexpected character: {t.value[0]}")
    t.lexer.skip(1)


lexer_exp = lex.lex()
lexer_exp2 = lex.lex()

# 定义语法分析器（Yacc）

precedence = (
    ('left', 'LESSTHAN', 'MORETHAN', 'LESSTHANEQ', 'MORETHANEQ', 'DEQUAL', 'NOTEQU', 'AND', 'OR', 'NOT'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS'),  # Unary minus operator
)


def p_logic_expression(p):
    '''logic_expression :  math_expression
                | logic_expression LESSTHAN logic_expression
                | logic_expression MORETHAN logic_expression
                | logic_expression LESSTHANEQ logic_expression
                | logic_expression MORETHANEQ logic_expression
                | logic_expression DEQUAL logic_expression
                | logic_expression NOTEQU logic_expression
                | logic_expression AND logic_expression
                | logic_expression OR logic_expression
                | NOT logic_expression
    '''

    if len(p) == 2:
        p[0] = p[1]

    if len(p) == 3:
        if p[2] == 0:
            p[0] = 1
        else:
            p[0] = 0
    if len(p) == 4:
        if p[2] == '<':
            if p[1] < p[3]:
                p[0] = 1
            else:
                p[0] = 0
        if p[2] == '>':
            if p[1] > p[3]:
                p[0] = 1
            else:
                p[0] = 0
        if p[2] == '<=':
            if p[1] <= p[3]:
                p[0] = 1
            else:
                p[0] = 0
        if p[2] == '>=':
            if p[1] >= p[3]:
                p[0] = 1
            else:
                p[0] = 0
        if p[2] == '==':
            if p[1] == p[3]:
                p[0] = 1
            else:
                p[0] = 0
        if p[2] == '!=':
            if p[1] != p[3]:
                p[0] = 1
            else:
                p[0] = 0
        if p[2] == '&&':
            if p[1] * p[3] != 0:
                p[0] = 1
            else:
                p[0] = 0
        if p[2] == '||':
            if (p[1] != 0) or (p[3] != 0):
                p[0] = 1
            else:
                p[0] = 0
        return p[0]


def p_expr_uminus(p):
    'math_expression : MINUS math_expression %prec UMINUS'
    p[0] = -p[2]


def p_math_expression(p):
    '''
    math_expression : math_expression PLUS term
               | math_expression MINUS term
               | term
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        if p[2] == '+':
            p[0] = p[1] + p[3]
        elif p[2] == '-':
            p[0] = p[1] - p[3]


def p_term(p):
    '''
    term : term TIMES factor
         | term DIVIDE factor
         | term MOD factor
         | factor
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        if p[2] == '*':
            p[0] = p[1] * p[3]
        elif p[2] == '/':
            if p[3] == 0:
                global state
                state = False
                print("除数为0错误！")
                p[0] = 0
                return
            p[0] = p[1] / p[3]
        elif p[2] == '%':
            p[0] = p[1] % p[3]


def p_factor(p):
    '''
    factor : NUMBER
           | FLOAT
           | IDENTIFIER-logic
           | index-logic
           | LPAREN math_expression RPAREN
    '''
    if len(p) == 2: p[0] = p[1]
    if len(p) == 4: p[0] = p[2]


def p_IDENTIFIER_logic(p):
    '''
    IDENTIFIER-logic : IDENTIFIER
    '''
    global state
    if symbol_table.lookup(p[1]):  # 如果是变量
        temp = symbol_table.lookup(p[1])["value"]
        if (type(temp) is not int) and (type(temp) is not float):
            state = False
            p[0] = 0
        else:
            p[0] = temp
    else:
        print(f"Undefined variable: {p[1]}")
        state = False
        p[0] = 0


def p_index_logic(p):
    '''
    index-logic : index
    '''
    global state
    if isinstance(p[1], int):  # 如果是整数
        p[0] = p[1]
    elif isinstance(p[1], float):  # 如果是实数
        p[0] = p[1]
    elif isinstance(p[1], ExpressionStatement) or isinstance(p[1], DeclAssgnStatement):  # 如果是可表达式语句
        global parser_logic
        p[0] = parser_logic.parse(str(p[1].expression), lexer=lex.lex())
        if not (isinstance(p[0], int) or isinstance(p[0], float)):
            state = False
            p[0] = 0


def p_index(p):
    '''
    index : IDENTIFIER LBRACK math_expression RBRACK
                | index LBRACK math_expression RBRACK
    '''
    global state
    if (type(p[1]) is str) and ("\"" not in p[1]):
        if symbol_table.lookup(p[1]):  # 如果是变量
            temp = symbol_table.lookup(p[1])["value"]
            if type(temp) is list: p[0] = temp[p[3]]
            if type(temp) is BlockStatement: p[0] = temp.statements[p[3]]
        else:
            print(f"Undefined variable: {p[1]}")
            state = False
    else:
        if type(p[1]) is list:
            p[0] = p[1][p[3]]
        elif type(p[1]) is BlockStatement:
            p[0] = p[1].statements[p[3]]
        else:
            state = False
            print("表达式解析错误-对非语句块的语句进行索引")


def p_text_expression(p):
    '''
    text_expression : text_expression PLUS text_expression
               | TEXT
               | IDENTIFIER
               | index-text
    '''
    global state
    if len(p) == 4:
        if (type(p[1]) is not str) or (type(p[3]) is not str):
            state = False
            return
        if "\"" in p[1] and "\"" not in p[3]: p[0] = p[1][0:-1] + p[3] + "\""
        if "\"" not in p[1] and "\"" in p[3]: p[0] = "\"" + p[1] + p[3][1:]
        if "\"" not in p[1] and "\"" not in p[3]: p[0] = "\"" + p[1] + p[3] + "\""
        if "\"" in p[1] and "\"" in p[3]: p[0] = p[1][0:-1] + p[3][1:]
    if len(p) == 2:
        if "\"" in p[1]:
            p[0] = p[1]
        else:
            if symbol_table.lookup(p[1]):  # 如果是变量
                p[0] = symbol_table.lookup(p[1])["value"]
                if type(p[0]) is not str:
                    state = False
                    p[0] = "\"\""
                elif "\"" not in p[0]:
                    p[0] = "\"" + p[0] + "\""
            else:
                print(f"Undefined variable: {p[1]}")
                state = False
                p[0] = "\"\""


def p_index_text(p):
    '''
    index-text : index
    '''
    global state
    if isinstance(p[1], str):  # 如果是整数
        p[0] = p[1]
        if "\"" not in p[0]:
            p[0] = "\"" + p[0] + "\""
    elif isinstance(p[1], ExpressionStatement) or isinstance(p[1], DeclAssgnStatement):  # 如果是可表达式语句
        global parser_text
        p[0] = parser_text.parse(str(p[1].expression), lexer=lex.lex())
        if not (isinstance(p[0], str)):
            state = False
            p[0] = "\"\""
        else:
            p[0] = p[1]
            if "\"" not in p[0]:
                p[0] = "\"" + p[0] + "\""
    else:
        state = False
        p[0] = "\"\""


def p_obj_expression(p):
    '''
    obj_expression : IDENTIFIER
               | index
               | list
    '''
    global state
    if type(p[1]) is str:
        if symbol_table.lookup(p[1]):  # 如果是变量
            p[0] = symbol_table.lookup(p[1])["value"]
        else:
            print(f"Undefined variable: {p[1]}")
            state = False
    else:
        p[0] = p[1]


def p_list(p):
    '''list : LBRACK elements RBRACK
            | LBRACK RBRACK
    '''
    if len(p) == 3: p[0] = []
    if len(p) == 4:
        p[0] = p[2]


def p_elements(p):
    '''elements : element COMMA elements
                | element
    '''
    if len(p) == 4:
        p[0] = [p[1]] + p[3]
    else:
        p[0] = [p[1]]


def p_element(p):
    '''element : NUMBER
               | list
               | FLOAT
               | TEXT
               | IDENTIFIER
               | other
    '''
    global state
    if (type(p[1]) is str) and ("\"" not in p[1]):
        if symbol_table.lookup(p[1]):  # 如果是变量
            p[0] = symbol_table.lookup(p[1])["value"]
            return
        else:
            print(f"Undefined variable: {p[1]}")
            state = False
    p[0] = p[1]


def p_other(p):
    '''other : logic_expression
                | text_expression
    '''
    p[0] = p[1]
    global state
    if type(p[1]) is str:
        state = True


def p_error(p):
    # print("Expression Parse - Syntax error")
    global state
    # print(7749)
    # print(p)
    state = False


parser_logic = yacc.yacc(start='logic_expression', debug=False)  # 逻辑表达式解析
parser_text = yacc.yacc(start='text_expression', debug=False)  # 文本表达式解析
parser_obj = yacc.yacc(start='obj_expression', debug=False)  # 文本表达式解析


class MyExpressionParser:
    def __init__(self, table: MySymbolTable):
        self.symbol_table = table
        # self.symbol_table.insert("x", "int", 10)
        # self.symbol_table.insert("y", "list", [20])
        # self.symbol_table.insert("z", "list", [[30]])
        self.count = 0

    def parser_exp(self, exp, obj=False, logic=False, text=False):
        global symbol_table
        symbol_table = self.symbol_table
        global parser_logic
        global parser_text
        global parser_obj
        # symbol_table.display()
        global state
        state = True
        if obj:
            result = parser_obj.parse(exp, lexer=lexer_exp)
            if state:
                return result
            else:
                return None
        if logic:
            result = parser_logic.parse(exp, lexer=lexer_exp)
            if state:
                return result
            else:
                return None
        if text:
            result = parser_text.parse(exp, lexer=lexer_exp)
            if state:
                return result
            else:
                return None
        result = parser_logic.parse(exp, lexer=lexer_exp)
        if state: return result
        state = True
        result = parser_text.parse(exp, lexer=lexer_exp)
        if state: return result
        state = True
        result = parser_obj.parse(exp, lexer=lexer_exp)
        if state: return result
        return None

    def compare_exp_type(self, s_type, value):
        if (type(value) is list) and (s_type == "list"): return True
        if (type(value) is int) and (s_type == "int"): return True
        if (type(value) is float) and (s_type == "real"): return True
        if (type(value) is str) and (s_type == "text"): return True
        return False


'''
my = MyExpressionParser(MySymbolTable())
my.symbol_table.insert("xx", "list", [DeclAssgnStatement("int", "a", 10)])
my.symbol_table.insert("yy", "list", [[100]])
r = my.parser_exp("[yy[0][0]]", obj=True)
print(r)
print(state)
'''


class Sync:
    def __init__(self):
        self.symbol_table = MySymbolTable()
        self.parent_table = MySymbolTable()
        self.parent_interpreter = None
        self.context_in = []
        self.context_out = []
        self.udil_temp = None
        self.jvm_path = "D:/Program/JDK21/bin/server/jvm.dll"
        self.javac_path = "D:/Program/JDK21/bin/javac"

    def sync_write(self, item, name, value):
        # print("成功执行 sync write")
        # print("item= " + item + "  name= " + name + " value= " + str(value))
        if type(item) is str:
            item = item.replace("\"", '')
        if type(name) is str:
            name = name.replace("\"", '')
        if type(value) is str:
            if item != "udil": value = value.replace("\"", '')
        if item == "program": self.sync_exec(name, value)
        if item == "statement": self.sync_eval(name, value)
        if item == "context-in-clear": self.context_in = []
        if item == "context-out-clear": self.context_out = []
        if item == "context-in":
            l = [name, value]
            if l not in self.context_in: self.context_in.append(l)
        if item == "context-out":
            l = [name, value]
            if l not in self.context_out: self.context_out.append(l)
        if (item == "context-current") and (type(self.parent_interpreter) is Interpreter):
            if name == "insert":  self.parent_interpreter.symbol_table.insert(value[0], value[1], value[2])
            if name == "update":  self.parent_interpreter.symbol_table.update(value[0], value[1], value[2])
            if name == "delete":  self.parent_interpreter.symbol_table.delete(value[0])
        if item == "java":
            if name == "javac": self.javac_path = value
            if name == "jvm": self.jvm_path = value
        if (item == "udil") and (type(self.parent_interpreter) is Interpreter):
            if name == "root": self.parent_interpreter.root = value
            if name == "text":
                parser = MyYacc()
                parser.debug = False
                parser.build()
                NodeAST = parser.exec(value)
                N = NodeAST.child[0]
                self.udil_temp = N.obj
            if name == "statement":
                if value:
                    self.udil_temp = value.text
                else:
                    self.udil_temp = None
                    print("获取无效对象！")
            if name == "pc_counter": self.parent_interpreter.pc_counter = value
            if name == "restart":
                if value == 1:
                    self.parent_interpreter.restart = True
                else:
                    self.parent_interpreter.restart = False

    def sync_read(self, item, name):
        # print("成功执行 sync read")
        # print("item= " + item + "  name= " + name)
        r = None
        if type(item) is str:
            item = item.replace("\"", '')
        if type(name) is str:
            name = name.replace("\"", '')
        if item == "context-out":
            for it in self.context_out:
                if name == it[0]:
                    r = it[1]
                    break
        if item == "context-current":
            t = self.parent_interpreter.lookup(name)
            if t:
                return 1
            else:
                return 0
        if (item == "udil") and (type(self.parent_interpreter) is Interpreter):
            if name == "pc_counter": r = self.parent_interpreter.pc_counter
            if name == "temp": r = self.udil_temp
        return r

    def sync_exec(self, compiler, program):
        if compiler == "python":
            code = ""
            for it in self.context_in:
                command = it[0] + "=" + str(it[1])
                code = code + command + "\n"
            code = code + program + "\nout = []"
            for it in self.context_out:
                code += "\nout.append(" + it[0] + ")\n"
            exec(code)
            output = eval("out")
            for n in range(len(output)):
                self.context_out[n][1] = output[n]
        if compiler == "udil":
            if type(program) is not BlockStatement:
                print("代码块错误！")
                return
            temp_iter = Interpreter(MySymbolTable())
            temp_table = temp_iter.symbol_table
            for it in self.context_in:
                var_name, var_value = it
                if type(var_value) is int: temp_table.insert(var_name, "int", var_value)
                if type(var_value) is float: temp_table.insert(var_name, "real", var_value)
                if type(var_value) is list: temp_table.insert(var_name, "list", var_value)
                if type(var_value) is str: temp_table.insert(var_name, "text", var_value)
                if type(var_value) is BlockStatement: temp_table.insert(var_name, "block", var_value)
            for states in program.statements:
                temp_iter.parse_statement(states)
            m = len(self.context_out)
            for n in range(m):
                self.context_out[n][1] = temp_table.lookup(self.context_out[n][0])["value"]
        if compiler == "java":
            class_pattern = re.compile(r'public\s+class\s+(\w+)\s*\{')
            matches = class_pattern.findall(program)
            class_name = matches[0]
            first_occur = program.find('{')
            extra_code = "\n"
            for it in self.context_in:
                name, value = it[0], it[1]
                if type(value) is int: extra_code += "static int " + name + " = " + str(value) + ";\n"
                if type(value) is float: extra_code += "static float " + name + " = " + str(value) + ";\n"
                if type(value) is str: extra_code += "static String " + name + " = " + str(value) + ";\n"
            for it in self.context_out:
                name, c_type = it[0], it[1]
                if c_type == "int": extra_code += "public static int qwertyuiop" + name + " (int zxcvbnm){return " + name + ";}\n"
                if c_type == "real": extra_code += "public static float qwertyuiop" + name + " (int zxcvbnm){return " + name + ";}\n"
                if c_type == "text": extra_code += "public static String qwertyuiop" + name + " (int zxcvbnm){return " + name + ";}\n"
            program = program[0:first_occur + 1] + extra_code + program[first_occur + 1:]
            Code_Path = os.getcwd() + "\\" + class_name + ".java"
            Class_Path = os.getcwd() + "\\" + class_name + ".class"
            try:
                with open(Code_Path, 'w') as file:
                    file.write(program)
                # print(f"成功保存文本到文件 {Code_Path}")
            except Exception as e:
                print(f"保存文本时发生错误: {e}")
            # 编译 Java 文件
            self.compile_java(Code_Path)
            # 启动JVM
            jpype.startJVM(self.jvm_path, "-ea", "-Dfile.encoding=utf-8", convertStrings=True, classpath=[os.getcwd()])
            SampleJavaClass = jpype.JClass(class_name)
            # 创建 Java 对象
            java_object = SampleJavaClass()
            java_object.main(None)
            for n in range(len(self.context_out)):
                self.context_out[n][1] = eval("java_object.qwertyuiop" + self.context_out[n][0] + "(0)")
            try:
                if os.path.exists(Code_Path):
                    os.remove(Code_Path)
                    # print(f"成功删除文件 {Code_Path}")
                else:
                    print(f"文件 {Code_Path} 不存在")
            except Exception as e:
                print(f"删除文件时发生错误: {e}")
            try:
                if os.path.exists(Class_Path):
                    os.remove(Class_Path)
                    # print(f"成功删除文件 {Class_Path}")
                else:
                    print(f"文件 {Class_Path} 不存在")
            except Exception as e:
                print(f"删除文件时发生错误: {e}")

    def sync_eval(self, context, statement):
        if context == "current":
            temp_iter = Interpreter(self.parent_table)
            temp_iter.parse_statement(statement)

    def compile_java(self, code_path):
        # 构建 javac 命令
        compile_command = [self.javac_path, code_path]
        try:
            # 调用 javac 编译 Java 文件
            subprocess.run(compile_command, check=True)
            # print(f"Java 文件 {code_path} 编译成功.")
        except subprocess.CalledProcessError as e:
            print(f"Java 文件 {code_path} 编译失败. 错误信息: {e.stderr}")


class Interpreter:
    def __init__(self, symbol_table: MySymbolTable, root=None):
        self.symbol_table = symbol_table
        self.debug = True
        self.expression_parser = MyExpressionParser(self.symbol_table)
        self.sync = Sync()
        self.sync.parent_interpreter = self
        self.root = root
        self.restart = False
        self.exec_index = False
        self.pc_counter = 0
        self.print_text = True

    def parse_program(self, program):
        parser = MyYacc()
        parser.debug = False
        parser.build()
        NodeAST = parser.exec(program)
        root = BlockStatement("root", [])
        self.pc_counter = 0
        self.root = root
        if NodeAST:
            for N in NodeAST.child:
                obj = N.obj
                self.root.statements.append(obj)
        else:
            print("解析程序文件失败！")

    def exec_root(self):
        loop_mark = True
        while loop_mark:
            loop_mark = False
            if self.debug: print("\n\n\n\n\n\n\n\n\n\n")
            if self.debug: print("Test begin")
            if self.debug: print()
            statements = self.root.statements
            m = -1
            for statement in statements:
                m += 1
                if self.exec_index:
                    if m < self.root.exec_counter: continue
                if m < self.pc_counter: continue
                result = self.parse_statement(statement)
                if self.debug: print(result)
                if self.debug: print()
                if self.restart: break
                self.pc_counter += 1
            if self.debug: self.symbol_table.display()
            if self.restart:
                self.restart = False
                loop_mark = True

    def parse_statement(self, statement):
        r2 = False
        self.sync.parent_table = self.symbol_table
        if type(statement) is DeclarationStatement:
            r2 = self.parse_declaration_statement(statement)
        if type(statement) is AssignmentStatement:
            r2 = self.parse_assignment_statement(statement)
        if type(statement) is BlockStatement:
            r2 = self.parse_block_statement(statement)
        if type(statement) is IfStatement:
            r2 = self.parse_if_statement(statement)
        if type(statement) is SYNCWriteStatement:
            r2 = self.parse_sync_write_statement(statement)
        if type(statement) is SYNCReadStatement:
            r2 = self.parse_sync_read_statement(statement)
        if type(statement) is DeclAssgnStatement:
            r2 = self.parse_decl_assgn_statement(statement)
        if type(statement) is ExpressionStatement:
            r2 = self.parse_expression_statement(statement)
        if type(statement) is WhileStatement:
            r2 = self.parse_while_statement(statement)
        if self.print_text: print("statement text:  " + statement.text)
        if r2 is False: print(f"Error Statement = {type(statement)} , Error Row = {statement.position} ")
        return r2

    def parse_declaration_statement(self, statement: DeclarationStatement):
        if self.debug: print("parse_declaration_statement ********************")
        if type(statement) is not DeclarationStatement:
            print("Error Statement Type!")
            return False
        var_name = statement.ID
        var_type = statement.data_type
        r3 = False
        # if self.debug: print("ID= " + var_name + "  " + "type= " + var_type)
        if var_type == "int":
            r3 = self.symbol_table.insert(var_name, var_type, 0)
        if var_type == "real":
            r3 = self.symbol_table.insert(var_name, var_type, 0.0)
        if var_type == "text":
            r3 = self.symbol_table.insert(var_name, var_type, "")
        if var_type == "list":
            r3 = self.symbol_table.insert(var_name, var_type, [])
        if var_type == "statement":
            r3 = self.symbol_table.insert(var_name, var_type, None)
        if r3 is False:
            print("编译错误：变量重复定义")
            return False
        return True

    def parse_decl_assgn_statement(self, statement: DeclAssgnStatement):
        if self.debug: print("parse_decl_assgn_statementt ********************")
        if type(statement) is not DeclAssgnStatement:
            print("Error Statement Type!")
            return False
        var_name = statement.ID
        var_type = statement.data_type
        var_value = None
        if var_type == "text":
            var_value = self.expression_parser.parser_exp(statement.expression, text=True)
        elif (var_type == "int") or (var_type == "real"):
            var_value = self.expression_parser.parser_exp(statement.expression, logic=True)
        else:
            var_value = self.expression_parser.parser_exp(statement.expression, obj=True)
        if var_value is None:
            return False
        r3 = False
        # if self.debug: print("ID= " + var_name + "  " + "type= " + var_type)
        if var_type == "int":
            r3 = self.symbol_table.insert(var_name, var_type, var_value)
        if var_type == "real":
            r3 = self.symbol_table.insert(var_name, var_type, var_value)
        if var_type == "text":
            r3 = self.symbol_table.insert(var_name, var_type, var_value)
        if var_type == "list":
            r3 = self.symbol_table.insert(var_name, var_type, var_value)
        if var_type == "statement":
            r3 = self.symbol_table.insert(var_name, var_type, var_value)
        if r3 is False:
            print("编译错误：变量重复定义")
            return False
        return True

    def parse_assignment_statement(self, statement: AssignmentStatement):
        if self.debug: print("parse_assignment_statement ********************")
        if type(statement) is not AssignmentStatement:
            print("Error Statement Type!")
            return False
        variable = statement.variable
        # if self.debug: print("variable= " + str(variable) + "  expression= " + str(statement.expression))
        if type(variable) is list:
            value = self.expression_parser.parser_exp(statement.expression)
            if value is None:
                return False
            var_name = variable[0]
            var_index = self.expression_parser.parser_exp(variable[1])
            var_index2 = 0
            if len(variable) == 3:
                var_index2 = self.expression_parser.parser_exp(variable[2])
            lookup = self.symbol_table.lookup(var_name)
            if lookup is None:
                return False
            if type(lookup["value"]) is not list:
                print("被索引对象不是列表，编译错误！")
                return False
            var_list = lookup["value"]
            if len(variable) == 2:
                if var_index >= len(var_list): var_list = var_list + [None] * (var_index + 1 - len(var_list))
                var_list[var_index] = value
            if len(variable) == 3:
                if var_index >= len(var_list): var_list = var_list + [[None] * (var_index2 + 1)] * (
                        var_index + 1 - len(var_list))
                var_list[var_index][var_index2] = value
            r = self.symbol_table.update(var_name, lookup["type"], var_list)
            if r:
                return True
            else:
                return False
        else:
            var_name = variable
            lookup = self.symbol_table.lookup(var_name)
            if lookup is None:
                return False
            value = None
            if lookup["type"] == "text":
                value = self.expression_parser.parser_exp(statement.expression, text=True)
            elif (lookup["type"] == "int") or (lookup["type"] == "real"):
                value = self.expression_parser.parser_exp(statement.expression, logic=True)
            else:
                value = self.expression_parser.parser_exp(statement.expression, obj=True)
            if value is None:
                return False
            type_compare = self.expression_parser.compare_exp_type(lookup["type"], value)
            if type_compare is False:
                print("编译错误-赋值语句错误-类型不匹配")
                return False
            r = self.symbol_table.update(var_name, lookup["type"], value)
            if r:
                return True
            else:
                return False

    def parse_block_statement(self, statement: BlockStatement):
        if self.debug: print("parse_block_statement ********************")
        if type(statement) is not BlockStatement:
            print("Error Statement Type!")
            return False
        r = self.symbol_table.insert(statement.ID, "block", statement)
        return r

    def parse_expression_statement(self, statement: ExpressionStatement):
        if self.debug: print("parse_expression_statement ********************")
        return True

    def parse_if_statement(self, statement: IfStatement):
        if self.debug: print("parse_if_statement ********************")
        if type(statement) is not IfStatement:
            print("Error Statement Type!")
            return False
        exp = self.expression_parser.parser_exp(statement.expression)
        r4 = True
        temp_table = MySymbolTable()
        temp_table.parent = self.symbol_table
        if exp != 0:
            main_block = statement.main_block
            self.symbol_table = temp_table
            for state in main_block:
                r = self.parse_statement(state)
                if r is False: r4 = False
            self.symbol_table = self.symbol_table.parent
        else:
            else_block = statement.else_block
            if else_block:
                self.symbol_table = temp_table
                for state in else_block:
                    r = self.parse_statement(state)
                    if r is False: r4 = False
                self.symbol_table = self.symbol_table.parent
        return r4

    def parse_while_statement(self, statement: WhileStatement):
        if self.debug: print("parse_while_statement ********************")
        if type(statement) is not WhileStatement:
            print("Error Statement Type!")
            return False
        exp = self.expression_parser.parser_exp(statement.expression)
        r5 = True
        temp_table = MySymbolTable()
        temp_table.parent = self.symbol_table
        while exp != 0:
            main_block = statement.main_block
            self.symbol_table = temp_table
            for state in main_block:
                r = self.parse_statement(state)
                if r is False: r5 = False
            self.symbol_table = self.symbol_table.parent
            exp = self.expression_parser.parser_exp(statement.expression)
        return r5

    def parse_sync_write_statement(self, statement: SYNCWriteStatement):
        if self.debug: print("parse_sync_write_statement ********************")
        if type(statement) is not SYNCWriteStatement:
            print("Error Statement Type!")
            return False
        key, value = statement.key, statement.value
        r = False
        if len(key) == 2:
            r = True
            key[0] = self.expression_parser.parser_exp(key[0])
            key[1] = self.expression_parser.parser_exp(key[1])
            value_out = self.expression_parser.parser_exp(value)
            temp = ""
            if type(key[0]) is str:
                temp = key[0].replace("\"", '')
                if temp == "statement": value_out = self.expression_parser.parser_exp(value, obj=True)
            self.sync.sync_write(key[0], key[1], value_out)
        return r

    def parse_sync_read_statement(self, statement: SYNCReadStatement):
        if self.debug: print("parse_sync_read_statement ********************")
        if type(statement) is not SYNCReadStatement:
            print("Error Statement Type!")
            return False
        key, value, s_type = statement.key, statement.value, statement.s_type
        r = False
        if len(value) == 2:
            r = True
            value[0] = self.expression_parser.parser_exp(value[0])
            value[1] = self.expression_parser.parser_exp(value[1])
            return_value = self.sync.sync_read(value[0], value[1])
            if s_type:
                rrr = self.symbol_table.insert(key, s_type, return_value)
                if rrr:
                    return True
                else:
                    print("编译错误 sync read 变量已重复定义")
                    return False
            l1 = self.symbol_table.lookup(key)
            if l1:
                if self.expression_parser.compare_exp_type(l1["type"], l1["value"]):
                    r = self.symbol_table.update(key, l1["type"], return_value)
                else:
                    r = False
                    print("编译错误 sync read 类型不一致")
            else:
                r = False
                print("编译错误 sync read 变量不存在")
        return r


# 声明赋值运行测试
RUN_DeclarationAssignment = '''
int a;
int b;
real c;
list d;
text e;
statement f;
b = 100;
c = 150.0;
d = [b, c, 200];
e = "Hello,World!";
real pi = 3.14;
list g = [[0],6,[8]];
g[0][0] = [2];
'''
RUN_DeclarationAssignment = RUN_DeclarationAssignment[1:]

# 代码块运行测试
RUN_CodeBlock = '''
program{
int a = 10;
9.9;
int c = 200;
}
int a;
a = program[0]+1;
statement d = program[2];
sync("statement", "current") = d;
'''
RUN_CodeBlock = RUN_CodeBlock[1:]

# 条件语句运行测试
RUN_IfStatement = '''
int a = 10;
if (1<2){
a = 20;
}
if (3==5){
a = 30;
}
'''
RUN_IfStatement = RUN_IfStatement[1:]

# 循环语句运行测试
RUN_WhileStatement = '''
int a = 10;
while (a<20){
a = a+1;
}
'''
RUN_WhileStatement = RUN_WhileStatement[1:]

# 错误提示运行测试
RUN_ErrorNotice = '''
int a;
b = 20;
'''
RUN_ErrorNotice = RUN_ErrorNotice[1:]

# 同步python运行测试
RUN_PythonEXEC = '''
int x = 10010;
sync("context-in", "a") = x;
sync("context-out", "b") = 0;
text pythonProgram = "
b = a + 76
";
sync("program", "python") = pythonProgram;
int y = sync("context-out", "b");
'''
RUN_PythonEXEC = RUN_PythonEXEC[1:]

# 同步udil运行测试
RUN_UdilEXEC = '''
program{
a = 100;
}
int b = 50;
sync("context-in", "a") = b;
sync("context-out", "a") = 0;
sync("program", "udil") = program;
b = sync("context-out", "a");
'''
RUN_UdilEXEC = RUN_UdilEXEC[1:]

# 同步java运行测试
RUN_JavaEXEC = '''
int a = 23;
sync("context-in", "input") = a;
sync("context-out", "output") = "int";
text pythonProgram = "
public class HelloWorld {
    static int output = 0;
    public static void main(String[] args) {
        System.out.println(2306);
        output = 2306;
    }
}
";
sync("program", "java") = pythonProgram;
int out = sync("context-out", "output");
'''
RUN_JavaEXEC = RUN_JavaEXEC[1:]

# 列表功能运行测试
RUN_ListFun = '''
list a = [];
list b = a;
b[0] = 100;
list c = b;
c[1][1] = 20;
list d = [c[0]];
'''
RUN_ListFun = RUN_ListFun[1:]

# 执行更改运行测试
RUN_PCIndex = '''
int a = 0;
int b = 0;
b = b+1;
if (b<5){
sync("udil", "pc_counter") = 2;
sync("udil", "restart") = 1;
}
'''
RUN_PCIndex = RUN_PCIndex[1:]

# 文本语句转换运行测试
RUN_TextStatSwap = '''
program{
a = a+1;
}
text b = "c = c+1;";
sync("udil", "text") = b;
statement d = sync("udil", "temp");
statement temp = program[0];
sync("udil", "statement") = temp;
text e = sync("udil", "temp");
'''
RUN_TextStatSwap = RUN_TextStatSwap[1:]


def RUN_Test(RUN_Code):
    interpreter = Interpreter(MySymbolTable())
    # interpreter.symbol_table.insert("x", "list", [1, 2, 3])
    parser = MyYacc()
    parser.debug = False
    parser.build()
    NodeAST = parser.exec(RUN_Code)
    print("\n\n\n\n\n\n\n\n\n\n")
    print("Test begin")
    print()
    for N in NodeAST.child:
        obj = N.obj
        # print(obj)
        result = False
        result = interpreter.parse_statement(obj)
        print(result)
        # interpreter.expression_parser.symbol_table.display()
        print()
    interpreter.symbol_table.display()
    print()
    print("Test end")


# 开始代码测试
# RUN_Test(RUN_UdilEXEC)
RUN_Test(RUN_PythonEXEC)

'''
inter = Interpreter(MySymbolTable())
inter.parse_program(RUN_TextStatSwap)
inter.exec_root()
'''
