import ply.lex as lex
import ply.yacc as yacc
import re
import turtle


# 词法分析器：t函数为主构建主体
class MyLexer:
    # 操作符
    operations = [
        'NUMBER',
        'FLOAT',
        'PLUS',
        'MINUS',
        'TIMES',
        'DIVIDE',
        'EQUAL',
        'MOD',
        'LPAREN',
        'RPAREN',
        'LBRACK',
        'RBRACK',
        'LBRACE',
        'RBRACE',
        'ID',
        'DEQUAL',
        'NOTEQU',
        'OR',
        'AND',
        'NOT',
        'MORETHAN',
        'LESSTHAN',
        'MORETHANEQ',
        'LESSTHANEQ',
        'SEMICOLON',
        'COMMA',
        'CHAR',
        'TEXT',
    ]
    # 操作符正则表达式匹配
    t_PLUS = r'\+'
    t_MINUS = r'-'
    t_TIMES = r'\*'
    t_DIVIDE = r'/'
    t_EQUAL = r'='
    t_MOD = r'%'
    t_LPAREN = r'\('
    t_RPAREN = r'\)'
    t_LBRACK = r'\['
    t_RBRACK = r'\]'
    t_LBRACE = r'\{'
    t_RBRACE = r'\}'
    t_ignore = ' \t'
    t_DEQUAL = r'=='
    t_NOTEQU = r'!='
    t_OR = r'\|\|'
    t_AND = r'&&'
    t_NOT = r'!'
    t_MORETHAN = r'>'
    t_LESSTHAN = r'<'
    t_MORETHANEQ = r'>='
    t_LESSTHANEQ = r'<='
    t_SEMICOLON = r';'
    t_COMMA = r','
    # 关键字，其匹配形式在函数 t_IDENTIFIER 里面体现
    keywords = {
        'if': 'if',
        'else': 'else',
        'while': 'while',
        'for': 'for',
        'return': 'return',
        'sync': 'sync',
        'Sync': 'Sync',
        'fun': 'funW',
        'Fun': 'Fun',
        'void': 'void',
        'break': 'break',
        'continue': 'continue',
        'int': 'int',
        'Integer': 'Integer',
        'real': 'real',
        'Real': 'Real',
        'char': 'char',
        'Char': 'Char',
        'text': 'text',
        'Text': 'Text',
        'list': 'listW',
        'List': 'ListW',
        'obj': 'objW',
        'Object': 'Object',
        'import': 'import',
        'block': 'blockW',
        'Block': 'BlockW',
        'statement': 'statementW',
        'Statement': 'StatementW',
    }
    # token集合 = 关键字集合 + 操作符集合
    tokens = operations + list(keywords.values())

    # 初始化词法分析器变量
    def __init__(self):
        self.debug = True
        self.result = None
        self.lexer = None
        self.text = ""

    # 解析标识符和关键字
    def t_ID(self, t):
        r'[a-zA-Z][a-zA-Z_0-9]*'
        t.type = self.keywords.get(t.value, 'ID')
        return t

    # 浮点数解析
    def t_FLOAT(self, t):
        r'(^-)?(\d)+(\.\d+)(?![a-zA-Z])'  # 普 通 浮 点 计 数
        t.type = 'FLOAT'
        # if t.value[-1] == ('f' or 'F') else 'DOUBLE '
        t.value = float(t.value[: -1] if t.value[-1] == ('f' or 'F') else t.value)
        return t

    # 整数解析
    def t_NUMBER(self, t):
        r'\d+(?![a-zA-Z])'
        t.value = int(t.value)
        return t

    # 字符解析
    quotation = "\'"
    rule = "([^" + quotation + r"\\]|\\.)*"

    @lex.TOKEN(quotation + rule + quotation)
    def t_CHAR(self, t):
        t.type = 'CHAR'
        return t

    # 文本解析
    quotation = "\""
    rule = "([^" + quotation + r"\\]|\\.)*"

    @lex.TOKEN(quotation + rule + quotation)
    def t_TEXT(self, t):
        t.type = 'TEXT'
        return t

    # 换行符处理，行号追踪
    def t_newline(self, t):
        r'\n+'  # 正则表达式进行符号匹配
        t.lexer.lineno += len(t.value)  # 不返回token对象t即视为丢弃该token

    # 换行符处理，列号追踪
    def find_column(self, input, token):
        last_cr = input.rfind('\n', 0, token.lexpos)
        column = (token.lexpos - last_cr)
        return column

    # 错误符处理，提示错误
    def t_error(self, t):
        print("Illegal character '%s'" % t.value[0])
        print(
            f"Illegal character '{t.value[0]}' at position: row={t.lexer.lineno},column={self.find_column(self.text, t)}")
        t.lexer.skip(1)  # 不返回token对象t即视为丢弃该token
        self.result = False  # 表示词法分析失败

    # 初始化词法分析器
    def build(self, **kwargs):
        self.lexer = lex.lex(module=self, **kwargs)
        self.result = True
        if self.debug: print("词法分析器初始化成功！")

    # 执行注释解析过程
    def comment(self, data):
        # 被注释部分替换成换行符和空格符即可，以免影响后续行号计算
        input = data
        while True:
            # 优先处理多行注释 形式为 /* comment(multi-lines) */
            comment_begin = re.search(r"/\*", input)
            comment_end = re.search(r"\*/", input)
            if comment_begin is None and comment_end is None:
                # 此处处理单行注释 形式为 // comment
                comment_index = re.search(r"//", input)
                if comment_index is None: return input
                index = comment_index.start()
                next_line = len(input)
                if "\n" in input[index:]:
                    next_line = input.index("\n", index)
                input = input[0:index] + input[next_line:len(input)]
                continue
            if (comment_begin is not None and comment_end is None) or (
                    comment_begin is None and comment_end is not None):
                self.result = False  # 表示词法分析失败
                if comment_begin is not None: index = comment_begin.start()
                if comment_end is not None: index = comment_end.start()
                row = 1 + len(re.findall(r"\n", input[0:index]))
                column = index - input[0:index].rfind("\n")
                print("词法错误：多行注释错误  错误位置：行号=" + str(row) + "，列号=" + str(column))
                break
            number_line = 0
            if "\n" in input[comment_begin.start():comment_end.start() + 2]:
                number_line = len(re.findall(r"\n", input[comment_begin.start():comment_end.start() + 2]))
            input = input[0:comment_begin.start()] + ("\n" * number_line) + input[comment_end.start() + 2:len(input)]
            return input

    # 执行词法分析过程
    def exec(self, data):
        # 先处理注释，注释分单行注释和多行注释,注意去掉注释的时候保留换行符，避免错误定位时行号不对
        data = self.comment(data)
        self.text = data
        words = []
        if self.debug: print("词法分析输入：\n" + data)
        if data is None:
            print("执行注释解析过程 异常退出")
            return
        self.lexer.input(data)
        if self.debug: print("非注释部分开始词法分析...")
        while True:
            tok = self.lexer.token()
            if not tok: break
            if self.debug: print(tok)
            words.append(tok)
        if self.debug:
            print("词法分析结果：")
            print(words)
        return words


# 语法分析器：p函数为主构建主体
class MyYacc:
    tokens = MyLexer.tokens
    precedence = (
        ('left', 'LESSTHAN', 'MORETHAN', 'LESSTHANEQ', 'MORETHANEQ', 'DEQUAL', 'NOTEQU', 'AND', 'OR', 'NOT'),
        ('left', 'PLUS', 'MINUS'),
        ('left', 'TIMES', 'DIVIDE'),
        ('right', 'UMINUS'),  # Unary minus operator
    )

    def __init__(self):
        self.lexer = None
        self.debug = True
        self.result = None
        self.yacc = None
        self.text = ""

    # 程序整体框架：由多条语句构成，基本单位是statement
    def p_program(self, p):
        '''program  : statements
        '''
        if self.debug: print("program")
        if p[1] is not None: p[0] = Node("program", p[1].child)

    # 多条语句框架：由多条语句构成，基本单位是statement
    def p_statements(self, p):
        '''statements  : statement
                    | statements statement
        '''
        if self.debug: print("statements")
        if len(p) == 2:
            p[0] = Node("statements", [p[1]])
        else:
            p[0] = Node("statements", p[1].child + [p[2]])

    # 语句整体框架：由许多种类语句组成，语句各具特色
    def p_statement(self, p):
        '''statement : block-statement
                    | if-statement
                    | while-statement
                    | for-statement
                    | return-statement
                    | assignment-statement
                    | declaration-statement
                    | expression-statement
                    | decl-assgn-statement
                    | fun-define-statement
                    | sync-write-statement
                    | sync-read-statement
        '''
        if self.debug: print("statement")
        p[0] = p[1]

    # 块语句：由标识符+代码块构成，匿名情况下无标识符
    def p_block_statement(self, p):
        '''block-statement :  ID block
                            | block
        '''
        if self.debug: print("block-statement")
        if len(p) == 2:
            p[0] = Node("block-statement", p[1].child)
            p[0].statement = True
            p[0].obj = BlockStatement("", p[1].obj)
            p[0].obj.position = p[1].extra
        else:
            p1 = Node("identifier", [p[1]])
            p[0] = Node("block-statement", [p1] + p[2].child)
            p[0].statement = True
            p[0].obj = BlockStatement(p[1], p[2].obj)
            p[0].obj.position = p.lineno(1)

    # 代码块中间结构：包含空块等三种基本情况
    def p_block(self, p):
        '''block : LBRACE  RBRACE
                | LBRACE statements RBRACE
                | LBRACE  RBRACE SEMICOLON
                | LBRACE statements RBRACE SEMICOLON
        '''
        if self.debug: print("block")
        if len(p) == 3:
            p[0] = Node("block", [Node("lbrace", [p[1]]), Node("rbrace", [p[2]])])
            p[0].extra = p.lineno(1)
            p[0].obj = []
            return
        p1 = Node("lbrace", [p[1]])
        p3 = Node("rbrace", [p[3]])
        if len(p) == 4:
            if type(p[2]) is not Node:
                p2 = Node("rbrace", [p[2]])
                p3 = Node("semicolon", [p[3]])
                p[0] = Node("block", [p1, p2, p3])
                p[0].extra = p.lineno(1)
                return
            p[0] = Node("block", [p1] + p[2].child + [p3])
            states = []
            for s in p[2].child:
                states.append(s.obj)
            p[0].obj = states
        if len(p) == 5:
            p[0] = Node("block", [p1] + p[2].child + [p3, Node("semicolon", [p[4]])])
            states = []
            for s in p[2].child:
                states.append(s.obj)
            p[0].obj = states
            p[0].extra = p.lineno(1)

    # if语句：表达式+代码块结构，两种基本情况
    def p_if_statement(self, p):
        '''if-statement  : if LPAREN expression RPAREN block
                        |  if LPAREN expression RPAREN block else block
        '''
        if self.debug: print("if_statement")
        p1 = Node("keyword", [p[1]])
        p2 = Node("lparen", [p[2]])
        p4 = Node("rparen", [p[4]])
        if len(p) == 6:
            p[0] = Node("if-statement", [p1, p2, p[3], p4] + p[5].child)
            p[0].statement = True
            p[0].obj = IfStatement(p[3].obj, p[5].obj)
            p[0].obj.position = p.lineno(1)
        if len(p) == 8:
            p[0] = Node("if-statement", [p[1], p[2], p[3], p[4], p[5], p[6], p[7]])
            p[0].statement = True
            p[0].obj = IfStatement(p[3].obj, p[5].obj, p[7].obj)
            p[0].obj.position = p.lineno(1)

    # while语句：表达式+代码块结构，一种基本情况
    def p_while_statement(self, p):
        '''while-statement  : while LPAREN expression RPAREN block
        '''
        if self.debug: print("while_statement")
        p1 = Node("keyword", [p[1]])
        p2 = Node("lparen", [p[2]])
        p4 = Node("rparen", [p[4]])
        p[0] = Node("while_statement", [p1, p2, p[3], p4] + p[5].child)
        p[0].statement = True
        p[0].obj = WhileStatement(p[3].obj, p[5].obj)
        p[0].obj.position = p.lineno(1)

    # for语句：声明/赋值+表达式+赋值/空
    def p_for_statement(self, p):
        '''for-statement : for LPAREN assignment SEMICOLON expression SEMICOLON assignment RPAREN block
                        | for LPAREN declaration SEMICOLON expression SEMICOLON assignment RPAREN block
                        | for LPAREN assignment SEMICOLON expression SEMICOLON RPAREN block
                        | for LPAREN declaration SEMICOLON expression SEMICOLON RPAREN block
        '''
        if self.debug: print("for_statement")
        if len(p) == 9: p[0] = Node("for_statement", [p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8]])
        if len(p) == 10: p[0] = Node("for_statement", [p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8], p[9]])
        p[0].obj.position = p.lineno(1)

    # 声明中间结构：辅助构成包含声明的相关语句
    def p_declaration(self, p):
        '''declaration : type ID
        '''
        if self.debug: print("declaration")
        p2 = Node("identifier", [p[2]])
        p[0] = Node("declaration", [p[1], p2])
        ty = p[1].child
        p[0].obj = [ty[0], p[2]]

    # 赋值中间结构：辅助构成包含赋值的相关语句
    def p_assignment(self, p):
        '''assignment  : ID EQUAL expression
                       | index EQUAL expression
        '''
        if self.debug: print("assignment")
        p2 = Node("equal", [p[2]])
        p[0] = Node("assignment", [p[1], p2, p[3]])
        if type(p[1]) is Node:
            p[0].obj = [p[1].extra] + [p[3].obj]
            p[0].child = p[1].child + [p2, p[3]]
        else:
            p[0].obj = [p[1], p[3].obj]
            p1 = Node("identifier", [p[1]])
            p[0].child[0] = p1

    # 类型中间结构：所有可能的类型，辅助构成
    def p_type(self, p):
        '''type     : int
                    | real
                    | char
                    | text
                    | listW
                    | void
                    | objW
                    | funW
                    | blockW
                    | statementW
        '''
        if self.debug: print("type")
        p[0] = Node("type", [p[1]])

    # return语句：函数返回等情况调用
    def p_return_statement(self, p):
        '''return-statement  : return expression SEMICOLON
                            |  return SEMICOLON
        '''
        if self.debug: print("return_statement")
        if len(p) == 3: p[0] = Node("return_statement", [p[1], p[2]])
        if len(p) == 4: p[0] = Node("return_statement", [p[1], p[2], p[3]])
        p[0].obj.position = p.lineno(1)

    # 赋值语句：两种基本情况，需要考虑空
    def p_assignment_statement(self, p):
        '''assignment-statement  : assignment SEMICOLON
        '''
        if self.debug: print("assignment_statement")
        p2 = Node("semicolon", [p[2]])
        p[0] = Node("assignment_statement", p[1].child + [p2])
        p[0].statement = True
        assign = p[1].obj
        p[0].obj = AssignmentStatement(assign[0], assign[1])
        p[0].obj.position = p.lineno(2)

    # 声明语句：
    def p_declaration_statement(self, p):
        '''declaration-statement : declaration SEMICOLON
        '''
        if self.debug: print("declaration_statement")
        p2 = Node("semicolon", [p[2]])
        p[0] = Node("declaration_statement", p[1].child + [p2])
        p[0].statement = True
        assign = p[1].obj
        p[0].obj = DeclarationStatement(assign[0], assign[1])
        p[0].obj.position = p.lineno(2)

    # 声明赋值语句：
    def p_decl_assgn_statement(self, p):
        '''decl-assgn-statement : type ID EQUAL expression SEMICOLON
        '''
        if self.debug: print("decl_assgn_statement")
        p2 = Node("identifier", [p[2]])
        p3 = Node("equal", [p[3]])
        p5 = Node("semicolon", [p[5]])
        p[0] = Node("decl_assgn_statement", [p[1], p2, p3, p[4], p5])
        p[0].statement = True
        p[0].obj = DeclAssgnStatement(p[1].child[0], p[2], p[4].obj)
        p[0].obj.position = p.lineno(2)

    # 表达语句：单纯只有表达式的语句
    def p_expression_statement(self, p):
        '''expression-statement  : expression SEMICOLON
        '''
        if self.debug: print("expression_statement")
        p2 = Node("semicolon", [p[2]])
        p[0] = Node("expression_statement", [p[1], p2])
        p[0].statement = True
        p[0].obj = ExpressionStatement(p[1].obj)
        p[0].obj.position = p.lineno(2)

    def p_expr_uminus(self, p):
        'expression : MINUS expression %prec UMINUS'
        p[0] = p[2]
        p[0].child = [Node("minus", [p[1]])] + p[2].child
        p[0].obj = "-" + p[0].obj

    # 表达式中间结构：
    def p_expression(self, p):
        '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | LPAREN expression RPAREN
                  | expression LESSTHAN expression
                  | expression MORETHAN expression
                  | expression LESSTHANEQ expression
                  | expression MORETHANEQ expression
                  | expression DEQUAL expression
                  | expression NOTEQU expression
                  | ID
                  | NUMBER
                  | FLOAT
                  | function
                  | CHAR
                  | TEXT
                  | list
                  | index
        '''
        if self.debug: print("expression")
        if len(p) == 2:
            p[0] = Node("expression", [p[1]])
            if type(p[1]) is Node:
                p[0].obj = p[1].obj
            else:
                p[0].obj = str(p[1])
            if type(p[1]) is Node and (p[1].info == "index"):
                p[0].obj = p[1].obj
        if len(p) == 4:
            p[0] = Node("expression", [p[1], p[2], p[3]])
            p[0].obj = p[1].obj + p[2] + p[3].obj

    # 列表中间结构：列表用[]表示，列表索引混淆，词法不易识别
    def p_list(self, p):
        '''list  : LBRACK elements RBRACK
                |  LBRACK RBRACK
        '''
        if self.debug: print("list")
        if len(p) == 3:
            p[0] = Node("list", [p[1], p[2]])
            p[0].obj = p[1] + p[2]
        if len(p) == 4:
            p[0] = Node("list", [p[1], p[2], p[3]])
            p[0].obj = p[1] + p[2].obj + p[3]

    def p_elements(self, p):
        '''elements  : expression COMMA elements
                | expression
        '''
        if self.debug: print("elements")
        if len(p) == 2:
            p[0] = Node("elements", [p[1]])
            p[0].obj = p[1].obj
        if len(p) == 4:
            p[0] = Node("elements", [p[1], p[2]] + p[3].child)
            p[0].obj = p[1].obj + p[2] + p[3].obj

    # 索引中间结构：需要与列表区分开
    def p_index(self, p):
        '''index  : ID LBRACK expression RBRACK
                | index LBRACK expression RBRACK
        '''
        if self.debug: print("index")
        p2 = Node("lbrack", [p[2]])
        p4 = Node("rbrack", [p[4]])
        if type(p[1]) is str:
            p1 = Node("identifier", [p[1]])
            p[0] = Node("index", [p1, p2, p[3], p4])
            p[0].extra = [p[1], p[3].obj]
            p[0].obj = p[1] + p[2] + p[3].obj + p[4]
        else:
            p[0] = Node("index", p[1].child + [p2, p[3], p4])
            p[0].obj = p[1].obj + p[2] + p[3].obj + p[4]
            p[0].extra = p[1].extra + [p[3].obj]

    # 可索引表达式：一部分表达式子集
    def p_expression_index(self, p):
        '''expression-index  : expression-index PLUS expression-index
                  | expression-index MINUS expression-index
                  | expression-index TIMES expression-index
                  | expression-index DIVIDE expression-index
                  | LPAREN expression-index RPAREN
                  | ID
                  | NUMBER
                  | function
                  | index
        '''
        if self.debug: print("expression-index")
        if len(p) == 2:
            p[0] = Node("expression-index", [p[1]])
            p[0].obj = str(p[1])
        if len(p) == 4:
            p[0] = Node("expression-index", [p[1], p[2], p[3]])
            p[0].obj = p[1].obj + p[2] + p[3].obj

    # 函数定义语句：函数参数+代码块结构
    def p_fun_define_statement(self, p):
        '''fun-define-statement  : type ID LPAREN params RPAREN  block
                                |  type ID LPAREN RPAREN  block
        '''
        if self.debug: print("fun_define_statement")
        if len(p) == 6: p[0] = Node("fun_define_statement", [p[1], p[2], p[3], p[4], p[5]])
        if len(p) == 7: p[0] = Node("fun_define_statement", [p[1], p[2], p[3], p[4], p[5], p[6]])
        p[0].obj.position = p.lineno(2)

    # 函数参数中间结构：参与函数定义语句
    def p_params(self, p):
        '''params  : type ID
                  |  type ID COMMA params
        '''
        if self.debug: print("params")
        if len(p) == 3: p[0] = Node("params", [p[1], p[2]])
        if len(p) == 5: p[0] = Node("params", [p[1], p[2], p[3], p[4]])

    # 函数：考虑参数为空的情况
    def p_function(self, p):
        '''function  : ID LPAREN params-call RPAREN
                    |  ID LPAREN RPAREN
        '''
        if self.debug: print("function")
        if len(p) == 4: p[0] = Node("function", [p[1], p[2], p[3]])
        if len(p) == 5: p[0] = Node("function", [p[1], p[2], p[3], p[4]])

    # 参数：
    def p_params_call(self, p):
        '''params-call  : expression
                    | expression COMMA params-call
        '''
        if self.debug: print("params_call")
        if len(p) == 2:
            p[0] = Node("params_call", [p[1]])
            p[0].obj = [p[1].obj]
        if len(p) == 4:
            p[0] = Node("params_call", [p[1], p[2]] + p[3].child)
            p[0].obj = [p[1].obj] + p[3].obj

    # 同步赋值语句：
    def p_sync_write_statement(self, p):
        '''sync-write-statement  : sync  LPAREN  params-call  RPAREN EQUAL  expression SEMICOLON
                                |  sync  LPAREN  RPAREN EQUAL  expression SEMICOLON
        '''
        if self.debug: print("sync_write_statement")
        p1 = Node("keyword", [p[1]])
        p2 = Node("lparen", [p[2]])
        if len(p) == 7:
            p3 = Node("rparen", [p[3]])
            p4 = Node("equal", [p[4]])
            p6 = Node("semicolon", [p[4]])
            p[0] = Node("sync_write_statement", [p1, p2, p3, p4, p[5], p6])
            p[0].statement = True
            p[0].obj = SYNCWriteStatement([], p[5].obj)
            p[0].obj.position = p.lineno(1)
        if len(p) == 8:
            p4 = Node("rparen", [p[4]])
            p5 = Node("equal", [p[5]])
            p7 = Node("semicolon", [p[7]])
            p[0] = Node("sync_write_statement", [p1, p2, p[3], p4, p5, p[6], p7])
            p[0].statement = True
            p[0].obj = SYNCWriteStatement(p[3].obj, p[6].obj)
            p[0].obj.position = p.lineno(1)

    # 同步取值语句：
    def p_sync_read_statement(self, p):
        '''sync-read-statement  : ID EQUAL sync  LPAREN params-call RPAREN  SEMICOLON
                                | ID EQUAL sync  LPAREN RPAREN  SEMICOLON
                                | type ID EQUAL sync  LPAREN params-call RPAREN  SEMICOLON
        '''
        if self.debug: print("sync_read_statement")
        p1 = Node("identifier", [p[1]])
        p2 = Node("equal", [p[2]])
        p3 = Node("keyword", [p[3]])
        p4 = Node("lparen", [p[4]])
        if len(p) == 7:
            p5 = Node("rparen", [p[5]])
            p6 = Node("semicolon", [p[6]])
            p[0] = Node("sync_read_statement", [p1, p2, p3, p4, p5, p6])
            p[0].statement = True
            p[0].obj = SYNCReadStatement(p[1], [])
            p[0].obj.position = p.lineno(1)
        if len(p) == 8:
            p6 = Node("rparen", [p[6]])
            p7 = Node("semicolon", [p[7]])
            p[0] = Node("sync_read_statement", [p1, p2, p3, p4, p[5], p6, p7])
            p[0].statement = True
            p[0].obj = SYNCReadStatement(p[1], p[5].obj)
            p[0].obj.position = p.lineno(1)
        if len(p) == 9:
            p2 = Node("identifier", [p[2]])
            p3 = Node("equal", [p[3]])
            p4 = Node("keyword", [p[4]])
            p5 = Node("lparen", [p[5]])
            p7 = Node("rparen", [p[7]])
            p8 = Node("semicolon", [p[8]])
            p[0] = Node("sync_read_statement", [p[1], p2, p3, p4, p5, p[6], p7, p8])
            p[0].statement = True
            p[0].obj = SYNCReadStatement(p[2], p[6].obj, s_type=p[1].child[0])
            p[0].obj.position = p.lineno(1)

    # 处理错误模块：
    def p_error(self, p):
        print("Syntax error in input!")
        self.result = False
        if p:
            pos = p.lexpos
            row = self.text[0:pos].count("\n") + 1
            column = pos - self.text[0:pos].rfind("\n")
            print(f"Syntax error at position: row={row},column={column}")
        else:
            print("Syntax error: unexpected end of input")

    # 初始化语法分析器
    def build(self):
        self.lexer = MyLexer()
        self.lexer.debug = self.debug
        self.lexer.build()
        self.lexer.debug = False
        self.yacc = yacc.yacc(module=self, debug=self.debug)
        self.result = True
        if self.debug: print("语法分析器初始化成功！")

    # 执行语法分析过程
    def exec(self, data):
        self.text = data
        parse = self.yacc.parse(self.lexer.comment(data))
        if self.debug:
            print("语法分析结果：")
            print(parse)
        self.yacc = None
        return parse

    def mylex(self, inp):
        self.lexer.input(inp)


# 语法树节点
class Node:
    def __init__(self, info, child=None, leaf=None):
        self.info = info  # 节点类型信息
        self.obj = None  # 节点对象信息
        self.extra = None  # 额外的信息
        self.statement = False  # 默认非语句
        # 子节点的列表
        if child:
            self.child = child
            if len(child) > 0:
                self.leaf = False
                return
        else:
            self.children = []
        # 是否叶子节点
        if leaf:
            self.leaf = leaf
        else:
            self.leaf = True

    # 将节点语法树转化为文本语法树
    def NodeAST2TextAST(self, obj):
        if type(obj) is not Node: return [str(obj), []]
        result = [obj.info, []]
        for x in obj.child:
            result[1].append(self.NodeAST2TextAST(x))
        return result

    # 利用递归求解文本语法树的高度
    def HeightOfTextAst(self, ast):
        if len(ast[1]) == 0: return 1
        tree_height = 0
        for sub_a in ast[1]:
            tree_height += self.HeightOfTextAst(sub_a)
        return tree_height


class IfStatement:
    def __init__(self, expression, main_block, else_block=None):
        self.expression = expression
        self.main_block = main_block
        self.else_block = else_block
        self.position = 0
        self.text = "if ( " + expression + " ) {\n"
        for s in main_block:
            self.text += s.text + "\n"
        if else_block:
            self.text += "}else{\n"
            for s in else_block:
                self.text += s.text + "\n"
        self.text += "}"


class WhileStatement:
    def __init__(self, expression, main_block):
        self.expression = expression
        self.main_block = main_block
        self.position = 0
        self.text = "while ( " + expression + " ) {\n"
        for s in main_block:
            self.text += s.text + "\n"
        self.text += "}"


class BlockStatement:
    def __init__(self, ID, statements=None):
        self.ID = ID
        self.position = 0
        self.exec_counter = 0
        self.text = ID + "{\n"
        if statements:
            self.statements = statements
        else:
            self.statements = []
        for s in self.statements:
            self.text += s.text + "\n"
        self.text += "}"


class AssignmentStatement:
    def __init__(self, variable, expression):
        self.variable = variable
        self.expression = expression
        self.position = 0
        self.text = ""
        if type(variable) is str:
            self.text = variable + " = " + expression + ";"
        else:
            index = variable[0]
            for i in range(1, len(variable)):
                index = index + "[" + variable[i] + "]"
            self.text = index + " = " + expression + ";"


class DeclarationStatement:
    def __init__(self, data_type, ID):
        self.data_type = data_type
        self.ID = ID
        self.position = 0
        self.text = data_type + " " + ID + ";"


class ExpressionStatement:
    def __init__(self, expression):
        self.expression = expression
        self.position = 0
        self.text = expression + ";"


class DeclAssgnStatement:
    def __init__(self, data_type, ID, expression):
        self.data_type = data_type
        self.ID = ID
        self.expression = expression
        self.position = 0
        self.text = data_type + " " + ID + " = " + expression + ";"


class SYNCWriteStatement:
    def __init__(self, key, value):
        self.key = key
        self.value = value
        self.position = 0
        self.text = "sync( "
        n = len(key)
        for i in range(0, n):
            self.text += key[i]
            if i != n - 1: self.text += ","

        self.text += ") = " + value + ";"


class SYNCReadStatement:
    def __init__(self, key, value, s_type=None):
        self.key = key
        self.value = value
        self.position = 0
        self.s_type = s_type
        self.text = key + " = sync( "
        if s_type: self.text = s_type + " " + self.text
        n = len(value)
        for i in range(0, n):
            self.text += value[i]
            if i != n - 1: self.text += ","

        self.text += ")" + ";"


def draw_multiway_tree(tree, x, y, width, height, degree):
    if tree:
        # 绘制当前节点
        turtle.goto(x, y)
        turtle.pencolor('red')
        turtle.write(tree[0], align="center", font=("Arial", 10, "normal"))
        turtle.pencolor('green')
        # 绘制子节点
        if len(tree) > 1:
            node = Node("")
            u_num = 0
            for m in range(0, len(tree[1])):
                turtle.penup()
                turtle.goto(x, y)
                turtle.pendown()
                m_count = node.HeightOfTextAst(tree[1][m])
                draw_multiway_tree(tree[1][m], x + width, y + u_num * (height / degree), width,
                                   m_count / degree * height, m_count)
                u_num += m_count


# 绘画语法树时请将画布全屏显示，否则部分绘画区域可能看不到
def drawAST(inputs, x, y, width, height):
    # 一个例子的多叉树结构
    sample_multiway_tree = inputs
    turtle.title('AST：抽象语法树结构')
    turtle.speed("fast")
    # 设置画笔
    turtle.pensize(1)
    turtle.speed(1)
    # 画多叉树
    turtle.penup()
    turtle.goto(x, y)
    turtle.pendown()
    node = Node("")
    draw_multiway_tree(sample_multiway_tree, x, y, width, height, node.HeightOfTextAst(sample_multiway_tree))
    # 隐藏画笔
    turtle.hideturtle()
    # 显示画面
    turtle.done()


# 赋值语句语法树测试
AST_AssignmentStatement = '''
a[0] = 50;
b = 100;
c = "Hello,World!";
'''
AST_AssignmentStatement = AST_AssignmentStatement[1:]

# 声明语句语法树测试
AST_DeclarationStatement = '''
int a;
real b;
list c;
text d;
'''
AST_DeclarationStatement = AST_DeclarationStatement[1:]

# 块语句语法树测试
AST_BlockStatement = '''
area1{
}
area2{
a = 100;
}
area3{
};
'''
AST_BlockStatement = AST_BlockStatement[1:]

# 条件语句语法树测试
AST_IfStatement = '''
if (1<2){
}
if (3==5){
int a;
}
'''
AST_IfStatement = AST_IfStatement[1:]

# 同步写语句语法树测试
AST_SyncWriteStatement = '''
sync(6) = 100;
sync() = 666;
sync("8", 4) = "Hello,World!";
'''
AST_SyncWriteStatement = AST_SyncWriteStatement[1:]

# 同步读语句语法树测试
AST_SyncReadStatement = '''
a = sync();
b = sync("1234");
'''
AST_SyncReadStatement = AST_SyncReadStatement[1:]
AST_SyncWriteStatement = AST_SyncWriteStatement[1:]

# 声明赋值语句语法树测试
AST_DeclAssgnStatement = '''
int a = 0;
real b = 1.0;
text c = "";
'''
AST_DeclAssgnStatement = AST_DeclAssgnStatement[1:]

# 表达语句语法树测试
AST_ExpressionStatement = '''
15;
"Hello";
'''
AST_ExpressionStatement = AST_ExpressionStatement[1:]

# 循环语句语法树测试
AST_WhileStatement = '''
while (1<2){
a = 100;
}
'''
AST_WhileStatement = AST_WhileStatement[1:]

# 其他语句语法树测试
AST_OtherStatement = '''
a = [[]];
'''
AST_OtherStatement = AST_OtherStatement[1:]


def AST_Test(AST_Code):
    parser = MyYacc()
    parser.build()
    NodeAST = parser.exec(AST_Code)
    N1 = NodeAST.child[0]
    print("Test begin")
    print(N1.obj)
    print("Test end")
    TextAST = None
    if NodeAST: TextAST = NodeAST.NodeAST2TextAST(NodeAST)
    print("文本语法树为：")
    print(TextAST)
    drawAST(TextAST, -600, -300, 150, 600)
    print("\n")
    if NodeAST:
        print("Accepted! ")
    else:
        print("Rejected !")
    print("___________________________________________________________________________\n")

# 开始语法树测试
# AST_Test(AST_WhileStatement)
