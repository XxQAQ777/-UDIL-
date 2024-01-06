from CodeCompiler import Interpreter, MySymbolTable

# UDIL示例程序
'''
该程序执行三个关键步骤：
    第一步：Udil调用Python，载入路径，Python从文本中读取Udil子程序，载出文本
    第二步：Udil调用Udil子程序，载入上下文，注册需要载出的符号变量
    第三步：Udil子程序调用Java，载入数值，Java进行数值计算，载出数值
'''
UDIL_Program_1 = '''
// 本语言支持注释
// 载入上下文，注册载出变量
text path = "'example.txt'";
sync("context-in", "path") = path;
sync("context-out", "program") = 0;
// 定义Python程序
text pythonProgram = "
def get_text(path):
    with open(path, \'r\') as file:
        content = file.read()
        return content
program = get_text(path)
";
// 执行Python，获取载出变量
sync("program", "python") = pythonProgram;
text program = sync("context-out", "program");
// 先清空之前的上下文信息
sync("context-out-clear", "var") = 0;
sync("context-in-clear", "var") = 0;
// 将文本转化为代码对象
sync("udil", "text") = program;
statement pro = sync("udil", "temp");	
// 执行Udil子程序
sync("context-in", "a") = 10;
sync("context-out", "a") = 0;
sync("program", "udil") = pro;
int result = sync("context-out", "a");
'''
UDIL_Program_1 = UDIL_Program_1[1:]

inter = Interpreter(MySymbolTable())
inter.parse_program(UDIL_Program_1)
inter.exec_root()
