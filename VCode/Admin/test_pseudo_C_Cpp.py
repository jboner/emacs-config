##############################################################################
# VoiceCode, a programming-by-voice environment
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
# (C)2000, National Research Council of Canada
#
##############################################################################

import os, profile, re
import sr_interface, vc_globals
import test_helpers
    
def dictate_pseudo_C_Cpp(commands):
    
    commands.open_file('blah.cpp')

    # macros

    commands.say(['wrap','header'])
    commands.say(['pound', 'include','quotes','string','after','quotes'])
    commands.say(['new','line'])
    commands.say(['pound', 'include','angle-brackets','string'])
    commands.say(['after','angle'])
    commands.say(['new','paragraph'])

    # comments

    commands.say(['comment','line'])
    commands.say(['test','comment'])
    commands.say(['new','paragraph'])
    commands.say(['begin','long','comment','this'])
    commands.say(['new','line'])
    commands.say(['is','an','important'])
    commands.say(['new','line'])
    commands.say(['bit','of','information','end','long','comment'])
    commands.say(['new','paragraph'])

    # class template

    commands.say(['define','template'])
    commands.say(['type','name'])
    commands.say(['some','data','type'])
    commands.say([',\\comma','class','object','type'])
    commands.say(['after','angle','new','line','define','class','foo'])
    commands.say(['class','body'])
    commands.say(['jump','out'])

    commands.say(['new','paragraph'])

    # declarations

    commands.say(['declare','function','test','procedure'])
    commands.say(['add','argument','integer','count'])
    commands.say([',\\comma','float','divisor'])
    commands.say(['returning','void'])
    commands.say(['new','statement'])
    commands.say(['declare','char','hello'])
    commands.say(['new','statement'])
    commands.say(['int\\int','count'])
    commands.say(['new','statement','integer','pointer','testing','pointers'])
    commands.say(['comma','pointer','testing','other','pointers'])
    commands.say(['new','statement','declare','the','value','of','type','double'])
    commands.say(['new','statement','declare','some','variable','of','type','static','int\\int'])
    commands.say(['new','statement','declare','char','star','star','string','address'])
    commands.say(['after','semi'])
    commands.say(['new','paragraph'])
    commands.say(['declare','function','foo'])
    commands.say(['returning','some','pointer'])
    commands.say(['add','argument','integer','counter','comma','sum','of','type','int\\int'])
    commands.say(['after','semi'])
    commands.say(['new','paragraph'])
    commands.say(['declare','method','bar','scope','operator','fabulous','method','returning','char','star'])
    commands.say(['add','argument','integer','pointer','another','memory','address'])
    commands.say(['after','semi','new','paragraph'])
    commands.say(['define','function','foo','returning','some','pointer'])
    commands.say(['add','argument','integer','counter','comma','sum','of','type','int\\int'])
    commands.say(['jump','out','jump','out','new','paragraph'])
    commands.say(['define','method','foo','scope','bar','returning','pointer','to','char'])
    commands.say(['method','body'])
    commands.say(['for','loop'])
    commands.say(['integer','index','equals','0\\zero'])
    commands.say(['after','semi','index'])
    commands.say(['is','less','or','equal','to'])
    commands.say(['platypus','count'])
    commands.say(['after','semi','index','increment'])
    commands.say(['loop','body'])

    # while ... do
    
    commands.say(['while','loop'])
    commands.say(['conditional','function','with','argument','index'])
    commands.say(['do', 'the', 'following', 'some','stuff'])
    commands.say(['empty','parens','semicolon'])
    commands.say(['after','semi','new','line'])

    # enumerator

    commands.say(['declare','enumerator','shapes'])
    commands.say(['after','brace','square',',\\comma','circle',',\\comma','triangle'])
    commands.say(['after','semi','new','paragraph'])

    # switch - case - default - break

    commands.say(['switch','initial','character','go', 'to','body'])
    commands.say(['case','single-quotes','a\\alpha'])
    commands.say(['after','colon','new','line','some','procedure','with','arguments','jump','out','semi','new','line','break'])
    commands.say(['case','single-quotes','b\\bravo'])
    commands.say(['after','colon','new','line','do','the', 'following','something','different','parens','jump','out','semi','new','line'])
    commands.say(['break','default','foo','bar','equals','4\\four','semi','jump','out','new','paragraph'])

    # typeid()

    commands.say(['if','type','I.','D.','of','some','variable','jump','out','is','equal','to','type','I.','D.','int\\int'])
    commands.say(['then','increment','some','variable','semicolon'])
    commands.say(['jump','out'])

    commands.say(['new','paragraph'])

    # try - catch
    commands.say(['try','value','equals','some','procedure','execution','with','arguments','jump','out','semicolon'])
    commands.say(['new','statement'])
    commands.say(['zoo','dot','animal','parens','quotes','tiger','jump','out','jump','out','dot','eat',
                  'with','argument','little','boy'])
    commands.say(['jump','out','jump','out','new','line'])
    commands.say(['catch','ellipsis'])
    commands.say(['go','to','body'])
    commands.say(['new','statement'])
    commands.say(['process','exception','parens','little','boy','jump','out','jump','out','new','paragraph'])

    # do...while
    commands.say(['do','while'])
    commands.say(['some','silly','value','increment','semi'])
    commands.say(['after','paren'])
    commands.say(['some','silly','value'])
    commands.say(['less','than'])
    commands.say(['10\\ten','jump','out','jump','out'])
    commands.say(['new','paragraph'])

    # if statement

    commands.say(['if','the','value','less','than','4\\four','then'])

    # type casts

    commands.say(['value','1\\one','equals','static','cast','integer','add','argument','some','symbol'])
    commands.goto_end_of_line()
    commands.say(['semi','new','statement'])
    commands.say(['value','2\\two','equals','reinterpret','cast','integer','add','argument','some','symbol'])
    commands.say(['new','statement'])
    commands.say(['value','3\\three','equals','dynamic','cast','integer','add','argument','some','symbol'])
    commands.say(['new','statement'])

    commands.say(['value','8\\eight','equals'])
    commands.say(['constant','cast'])
    commands.say(['integer','add','argument','some','symbol'])
    commands.say(['new','statement','above'])

    commands.say(['value','4\\four','equals','some','symbol'])
    commands.say(['constant','cast','to'])
    commands.say(['integer'])
    commands.say(['new','statement'])

    commands.say(['value','5\\five','equals','some','symbol','dynamic','cast','to','void','pointer'])
    commands.say(['new','statement'])
    commands.say(['value','7\\seven','equals','some','symbol','reinterpret','cast','to','fake','object','type'])
    commands.say(['new','statement','above'])
    commands.say(['value','6\\six','equals','some','symbol','static','cast','to','void','star'])

    commands.say(['next','semi'])
    commands.say(['next','semi'])
    commands.say(['again'])

    commands.say(['new','statement'])
    commands.say(['yet','another','value','equals','value','5\\five','cast','to','int\\int','pointer'])
    commands.say(['new','statement'])
    commands.say(['yet','another','value','equals','cast','int\\int','pointer','jump','out','value','8\\eight'])

    commands.say(['after','semi','new','line'])
    commands.say(['break'])

    # else-if and else

    commands.say(['else','if','the','value','greater','than','15\\fifteen','then','foo','equals','new','bar','semicolon'])
    commands.say(['else','continue'])

    commands.say(['new','statement','above','testing','adding','above','with','arguments'])

    # class definition

    commands.say(['jump','out','jump','out','jump','out','jump','out'])
    commands.say(['new','paragraph'])
    commands.say(['define','class','really','great','stuff'])
    commands.say(['with','superclass','important','superclass'])
    commands.say(['private','members'])
    commands.say(['new','line', 'int\\int','foo','semicolon'])
    commands.say(['jump','back','out'])
    commands.say(['new','public','member','new','line','float','pointer','bar','semi','new','line'])
    commands.say(['declare','function','testing','declaration'])

    # typedef struct

    commands.say(['jump','out'])    
    commands.say(['jump','out'])
    commands.say(['new','paragraph'])
    commands.say(['typedef','struct','my','tree','structured','data'])
    commands.say(['body'])
    commands.say(['character','star','description','semi','new','statement'])
    commands.say(['integer','value','count'])
    commands.say(['new','statement'])
    commands.say(['structured','tree','node','asterisk','left','child'])
    commands.say(['new','statement'])
    commands.say(['structured','tree','node','pointer','right','child'])
    commands.say(['jump','out'])
    commands.say(['structured','tree','node','comma','star','structured','tree','node'])
    commands.say(['after','semi','new','paragraph'])

    # union

    commands.say(['declare','union','united','states'])
    commands.say(['new','statement'])
    commands.say(['int\\int','number','of','states'])
    commands.say(['new','statement'])
    commands.say(['float','input','literacy','rate'])
    commands.say(['new','statement'])
    commands.say(['char','star','president'])
    commands.say(['jump','out','sample','type'])
    commands.say(['after','semi','new','paragraph'])

    commands.quit(save_speech_files=0, disconnect=0)


def test_dictate_from_scratch(testing):

    #
    # This file contains the native code that we will dictate
    #
    native_C_Cpp_file = vc_globals.test_data + os.sep + 'native_C_Cpp.cpp'

    #
    # Dictate some pseudo C/C++ where all symbols are already known
    #
#     print '>>> Dictating C/C++ when all symbols are known <<<\n'
#     testing.init_simulator_regression()
#     names = testing.namespace()
#     commands = names['commands']
#     commands.compile_symbols([native_C_Cpp_file])
#     commands.print_symbols()
#     dictate_pseudo_C_Cpp(commands)

    #
    # Dictate some pseudo C/C++ where only standard symbols are already known
    #
    print '\n>>> Dictating C/C++ when only standard symbols are known <<<\n'    
    testing.init_simulator_regression()
    names = testing.namespace()
    commands = names['commands']
#    commands.print_symbols()
    dictate_pseudo_C_Cpp(commands)    


def do_edit_test(testing, edit_file, test_fct, descr=None):
   names = testing.namespace()
   commands = names['commands']
   
   print '\n*********************\n*** Executing edit test: %s ***\n*********************\n' % descr

   commands.open_file(edit_file)
   test_fct(commands)
   
   a_match = re.search("([^\\\/]*$)", edit_file)
   commands.close_buffer(a_match.group(1), save=-1)
   
   print '\n*********************\n*** DONE with edit test: %s ***\n*********************\n' % descr
    
def test_editing(testing):
   #
   # This file contains code that will be edited in different ways
   #
   edit_file = vc_globals.test_data + os.sep + 'edit_this_buff.cpp'
   testing.init_simulator_regression()
   commands = testing.namespace()['commands']
   symbols = testing.mediator().interp.known_symbols
   export_file = os.path.join(vc_globals.test_data,
   'abbrevs.test_editing.init')
   symbols.export_abbreviations(export_file)
   commands.compile_symbols([edit_file])   
   export_file = os.path.join(vc_globals.test_data,
       'abbrevs.test_editing.postcompile')
   symbols.export_abbreviations(export_file)
   
   do_edit_test(testing, edit_file, insert_import_statement_test, 'insert an import statement in middle of a file')
   do_edit_test(testing, edit_file, create_new_class_test, 'create new class')
   do_edit_test(testing, edit_file, change_subclass_of_existing_class, 'change subclass of existing class')
   do_edit_test(testing, edit_file, add_method_to_existing_class_test, 'add_method_to_existing_class_test')
   do_edit_test(testing, edit_file, add_argument_to_existing_method_test, 'add_argument_to_existing_method_test')   
   do_edit_test(testing, edit_file, change_existing_argument_of_a_method_test, 'change_existing_argument_of_a_method_test')      
   do_edit_test(testing, edit_file, insert_line_of_code_in_method_test, 'insert_line_of_code_in_method_test')   
   do_edit_test(testing, edit_file, change_arguments_in_method_call_test, 'change_arguments_in_method_call_test')   
   do_edit_test(testing, edit_file, nested_if_then_else_test, 'nested_if_then_else_test')   
   do_edit_test(testing, edit_file, add_else_clause_test, 'add_else_clause_test')
   do_edit_test(testing, edit_file, add_except_clause_test, 'add_except_clause_test')
   export_file = os.path.join(vc_globals.test_data,
       'abbrevs.test_editing.aftertest')
   symbols.export_abbreviations(export_file)
   
def test_C_Cpp_compilation(testing):
   test_helpers.compilation_test(testing.mediator().interpreter(), os.path.join(vc_globals.test_data, 'used_to_test_C_Cpp_parsing.cpp'))   
   
# To create an edit test scenario, just   define a test function and invoke 
# it through do_edit_test as below.
#   do_edit_test(testing, edit_file, test_function, descr)    

###################################################################
# Misc C/C++ statements
###################################################################

def test_misc_C_Cpp_statements(testing):

    testing.init_simulator_regression()
    names = testing.namespace()
    commands = names['commands']

    commands.open_file('blah.cpp')
    commands.say(['define', 'class', 'some', 'variable', 'class', 'body'], user_input="1\n0\n0\n")
    commands.say(['define', 'method', 'some', 'method', 'add', 'arguments'], user_input="1\n0\n0\n")   
    commands.say(['collect', 'arguments', 'positional', 'arguments', 'comma'], user_input="1\n0\n0\n")      
    commands.say(['collect', 'keyword', 'arguments', 'keyword', 'arguments'], user_input="1\n0\n0\n") 
    commands.say(['class', 'body'], user_input="0\n0\n0\n")         
    commands.say(['some', 'array', 'equals', 'some', 'other', 'array', 
                  'sliced', 'at', 'one', 'colon', 'five', 'new', 'statement'], user_input="1\n1\n0\n")   
    commands.say(['some', 'dictionary', 'item', 'with', 'key', 'zero', 'jump', 'out', 
                  'equals', 'one'], user_input="1\n0\n0\n")
    commands.say(['comment', 'above'])
    commands.say(['this', 'is', 'a', 'commented', 'out'], user_input='1\n0\n0\n')
    

###################################################################
# C/C++ editing test functions
###################################################################

def insert_import_statement_test(commands):
   commands.goto_line(2)
   commands.say(['import', 'some', 'module'], user_input="1\n")
   
   
def create_new_class_test(commands):   
   commands.goto_line(20)
   commands.say(['define','class', 'dummy', 'class', 'body'], user_input="1\n")
   commands.say(['define', 'method', 'new', 'method', 'method', 'body', 'pass'], user_input="1\n")

def change_subclass_of_existing_class(commands):
   commands.goto_line(4)
   commands.say(['select', 'ASuper\\a super'], never_bypass_sr_recog=1)
   commands.say(['new', 'super', 'class'], user_input="1\n")
   
def add_method_to_existing_class_test(commands):
   commands.goto_line(19)
   commands.say(['add', 'method', 'some', 'method', 'method', 'body', 'pass'], user_input="1\n")

def add_argument_to_existing_method_test(commands):
   commands.goto_line(7)
   commands.say(['add', 'argument', 'extra', 'argument'], user_input="1\n")
   
def change_existing_argument_of_a_method_test(commands):
   commands.goto_line(7)
   commands.say(['select', 'some_argument\\some argument'],
       never_bypass_sr_recog=1)
   commands.say(['new', 'argument'], user_input="1\n")
   
def insert_line_of_code_in_method_test(commands):
   commands.goto_line(8)   
   commands.say(['new', 'statement'])
   commands.say(['some', 'array', 'equals', 'none'])
   
def change_arguments_in_method_call_test(commands):
   pass   
   commands.goto_line(10)
   commands.say(['select', 'some_array\\some array'], never_bypass_sr_recog=1)
   commands.say(['none'])
   commands.goto_line(11)
   commands.say(['after', 'paren', 'none'], never_bypass_sr_recog=1)

def nested_if_then_else_test(commands):
   commands.goto_line(11)
   commands.say(['new', 'statement', 'if', 'some', 'flag', 'then'], user_input="1\n1\n")
   commands.say(['do', 'the', 'following', 'some', 'more', 'stuff', 'with', 'arguments', 'some', 'argument'], user_input="1\n1\n")
   commands.say(['else', 'do', 'the', 'following', 'some', 'stuff', 'again', 'with', 'arguments', 'some', 'other', 'argument'], user_input="1\n1\n")
   commands.say(['bug', 'below', 'dot', 'following', 'one', 'will', 'not', 'be', 'inserted', 'at', 'the', 'right', 'level'], user_input="1\n1\n1\n1\n1\n1\n1\n1\n1\n")
   commands.say(['else', 'do', 'the', 'following', 'some', 'stuff', 'without', 'arguments'])
   
   
def add_else_clause_test(commands):
   commands.goto_line(11)
   commands.say(['else', 'clause'])

def add_except_clause_test(commands):   
   commands.goto_line(26)
   commands.say(['catch', 'exceptions'])
   

   

   
