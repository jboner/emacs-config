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
    
def dictate_pseudo_python(commands):
    
    #
    # These words must be in the SR vocab, otherwise some of the say()
    # statements will faile
    #
    sr_interface.addWord(sr_interface.vocabulary_entry('aliases', 'aliases'))
    sr_interface.addWord(sr_interface.vocabulary_entry('globals', 'globals'))

    commands.open_file('blah.py')
        
# causes Natspeak v.4 bug
    commands.say(['import\\import modules', 'O.', 'S.', ',\\comma', 'R.', 'E.', ',\\comma', 'string', ',\\comma', 'system', 'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
# causes Natspeak v.4 bug
    commands.say(['import\\import modules', 'auto\\auto', 'test', ',\\comma', 'natural', 'link', ',\\comma', 'V.', 'C.', 'globals', 'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)

   
#    commands.say(['from', 'module', 'actions', 'C.', 'C.', 'P.', 'P.', ' import all\\import all', 'new', 'statement'] , user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
# The above won't work now that we've eliminated leading and trailing
# spaces from the written forms entered as vocabular words.  try this instead
    commands.say(['from', 'module', 'actions', 'C.', 'C.', 'P.', 'P.', 'import all', 'new', 'statement'] , user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['from', 'module', 'application', 'state', 'import', 'application', 'state', 'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['from', 'module', 'context', 'generic', 'import', 'symbols', 'context', 'C.', 'comma', 'context', 'python', 'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['from', 'module', 'context', 'sensitive', 'command', 'import', 'symbols', 'context', 'sensitive', 'command', 'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
#    commands.print_symbols(['EdSim', 'ed_simulator'])
    commands.say(['from', 'module', 'Ed', 'simulator', 'import\\import symbol', 'Ed', 'simulator', 'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
#    commands.print_symbols(['EdSim', 'ed_simulator'])
    
    commands.say(['from', 'module', 'object', 'import', 'symbol', 'object', 'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['import\\import modules', 'Ed', 'simulator', 'comma', 'symbol', 'dictionary', 'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
# causes Natspeak v.4 bug
# also v. 6, now
#    commands.say(['import', 'module',  'S.', 'R.', 'interface',  'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
# try this
    commands.say(['import\\import module',  'S.', 'R.', 'interface',  'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['define', 'class', 'command', 'interpreter', 'sub class\\sub class', 'of', 'object', 'class', 'body'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['define', 'method', 'initialize', 'add', 'argument', 'on', 'application', 'equals', 'none', 'comma'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['symbol', 'dictionary', 'pickle', 'file', 'equals', 'none', 'comma', 'double', 'asterisk', 'attributes', 'method', 'body'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['self', 'dot', 'declare', 'attributes', 'with', 'arguments', 'brace', 'pair'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['single', 'quotes', 'un', 'translated', 'text', 'start', 'jump', 'out', ':\\colon', 'none', 'comma'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['single', 'quotes', 'un', 'translated', 'text', 'end', 'jump', 'out', ':\\colon', 'none', 'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['self', 'dot', 'deep', 'construct', 'with', 'arguments', 'command', 'interpreter', 'comma', 'continue', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['brace', 'pair', 'single', 'quotes', 'on', 'application', 'jump', 'out', ':\\colon', 'on', 'application', 'comma',], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['single', 'quotes', 'known', 'symbols', 'jump', 'out', ':\\colon', 'symbol', 'dictionary', 'dot', 'symbol', 'dictionary', 'without', 'arguments', 'comma', 'continue', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    
    commands.say(['single', 'quotes', 'language', 'specific', 'aliases', 'jump', 'out', ':\\colon', 'empty', 'dictionary', 'comma', 'continue', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['single', 'quotes', 'last', 'loaded', 'language', 'jump', 'out', ':\\colon', 'none', 'comma', 'continue', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['single', 'quotes', 'symbol', 'dictionary', 'pickle', 'file', 'jump', 'out', ':\\colon', 'symbol', 'dictionary', 'pickle', 'file', 'jump', 'out', 'comma', 'continue', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['attributes', 'new', 'statement', 'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
   
    commands.say(['back indent', 'define', 'method', 'spoken', 'form', 'regular', 'expression', 'add', 'argument', 'spoken', 'form'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
        
    commands.say(['method', 'body'], echo_utterance=1)
    
    commands.say(['words', 'equals', 'R.', 'E.', 'dot', 'split', 'with', 'arguments'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['single', 'quotes',  'back slash S.', 'plus', 'sign', 'jump', 'out', 'comma', 'spoken', 'form', 'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['regular', 'expression', 'equals', 'empty', 'single', 'quotes', 'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['for', 'loop', 'a', 'word', 'in', 'list', 'words', 'loop', 'body'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['first', 'equals', 'a', 'word', 'at', 'index', '0\\zero', 'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['rest', 'equals', 'a', 'word', 'at', 'index', '1\\one', ':\\colon', 'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['regular', 'expression', 'this', 'word', 'equals', 'single', 'quotes'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['open', 'bracket', 'jump', 'out', 'plus', 'string', 'dot', 'lower', 'with', 'arguments', 'first'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['jump', 'out', 'plus', 'string', 'dot', 'upper', 'with', 'arguments', 'first', 'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['if', 'statement', 'not', 'regular', 'expression', 'equal', 'to', 'empty', 'single', 'quotes', 'if', 'body'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['regular', 'expression', 'equals', 'regular', 'expression', 'plus', 'single', 'quotes', 'back slash S.', 'asterisk', 'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['regular', 'expression', 'equals', 'regular', 'expression', 'plus', 'regular', 'expression', 'this', 'word', 'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['return', 'regular', 'expression'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
    
    commands.say(['back indent', 'new statement'], echo_utterance=1)    

    commands.say(['if', 'not', 'this', 'word', 'then', 'this', 'word', 'equals', 'single', 'quotes', 'hello'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)

    commands.say(['else', 'if', 'this', 'word', 'is', 'equal', 'to', 'hi', 'then'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)

    #beg
#    commands.say(['else', 'if', 'this', 'word', 'is', 'equal', 'to', 'hi'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)
#    commands.say(['then'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)    
    #end

    commands.say(['this', 'word', 'equals', 'greetings', 'else'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)

    commands.say(['this', 'word', 'equals', 'single', 'quotes', 'done', 'new', 'statement'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)

    commands.say(['try', 'some', 'function', 'with', 'arguments'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)

    commands.say(['except', 'do', 'the', 'following', 'print', 'single', 'quotes', 'error'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)

    commands.say(['finally', 'do', 'print', 'single', 'quotes', 'all', 'right'], user_input='1\n1\n1\n1\n1\n1\n1\n', echo_utterance=1)        
    
    commands.quit(save_speech_files=0, disconnect=0)


def test_dictate_from_scratch(testing):

    #
    # This file contains the native code that we will dictate
    #
    native_py_file = vc_globals.test_data + os.sep + 'native_python.py'

    #
    # Dictate some pseudo python where all symbols are already known
    #
    print '>>> Dictating Python when all symbols are known <<<\n'
    testing.init_simulator_regression()
    names = testing.namespace()
    commands = names['commands']
    commands.compile_symbols([native_py_file])
    dictate_pseudo_python(commands)

    #
    # Dictate some pseudo python where only standard symbols are already known
    #
    print '\n>>> Dictating Python when only standard symbols are known <<<\n'    
    testing.init_simulator_regression()
    names = testing.namespace()
    commands = names['commands']
    dictate_pseudo_python(commands)    


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
   edit_file = vc_globals.test_data + os.sep + 'edit_this_buff.py'
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
   
def test_python_compilation(testing):
   test_helpers.compilation_test(testing.mediator().interpreter(), os.path.join(vc_globals.test_data, 'used_to_test_python_parsing.py'))   
   
# To create an edit test scenario, just   define a test function and invoke 
# it through do_edit_test as below.
#   do_edit_test(testing, edit_file, test_function, descr)    

###################################################################
# Misc python statements
###################################################################

def test_misc_py_statements(testing):

    testing.init_simulator_regression()
    names = testing.namespace()
    commands = names['commands']

    commands.open_file('blah.py')
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
# Python editing test functions
###################################################################

def insert_import_statement_test(commands):
   commands.goto_line(2)
   commands.say(['import', 'some', 'module'], user_input="1\n")
   
   
def create_new_class_test(commands):   
   commands.goto_line(20)
   commands.say(['class', 'dummy', 'class', 'body'], user_input="1\n")
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
   commands.say(['do', 'some', 'more', 'stuff', 'with', 'arguments', 'some', 'argument'], user_input="1\n1\n")
   commands.say(['else', 'do', 'some', 'stuff', 'again', 'with', 'arguments', 'some', 'other', 'argument'], user_input="1\n1\n")
   commands.say(['bug', 'below', 'dot', 'following', 'one', 'will', 'not', 'be', 'inserted', 'at', 'the', 'right', 'level'], user_input="1\n1\n1\n1\n1\n1\n1\n1\n1\n")
   commands.say(['else', 'do', 'some', 'stuff', 'without', 'arguments'])
   
   
def add_else_clause_test(commands):
   commands.goto_line(11)
   commands.say(['else', 'clause'])

def add_except_clause_test(commands):   
   commands.goto_line(26)
   commands.say(['catch', 'exceptions'])
   

   

   
