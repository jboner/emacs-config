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

"""Define a series of regression tests for VoiceCode

**NOTE:** This file must be run with execfile, not imported.  Also,
before the tests are actually run (with auto_test), you must create an
appropriate regression.PersistentMediator object with a reference to the
namespace in which you perform(ed) the execfile, and ensure that
this namespace contains a reference to this PersistentMediator
object named 'testing'
"""

import os
import sys
import time
import posixpath
import unittest
import NavigationWithinBufferTest
import MediatorConsoleWXTests
import actions_C_Cpp
import actions_py
import CmdInterp
import CSCmd
import cont_gen
import EdSim
import Object
import SymDict
import test_pseudo_python
import test_pseudo_C_Cpp
import util
import unit_testing
import vc_globals
import wxWindowsWithHelpersTest
import AppMgr
import RecogStartMgr
import GramMgr
import sr_grammars
import KnownTargetModule
import NewMediatorObject
import TargetWindow
import WinIDClient
import test_helpers
import CmdInterpTest
import SwitchBufferTest
import SymDictTest
import debug
import DiffCrawler
import difflib
import TextModeTest
import VoiceCodeRootTest
from pyUnitExample import SampleTestCase

from config_helpers import alpha_bravo

from actions_gen import *
from actions_C_Cpp import *
from actions_py import *
from cont_gen import *
from exceptions import Exception

# language context instances:
for lang in all_languages:
   exec('cont%s = ContLanguage("%s")'% (lang.capitalize(), lang))
contAnyLanguage = ContLanguage(all_languages)
contCStyleLanguage = ContLanguage(c_style_languages)

if_else_c = vc_globals.test_data + os.sep + 'ifelse.c'
small_buff_c = vc_globals.test_data + os.sep + 'small_buff.c'
small_buff_py = vc_globals.test_data + os.sep + 'small_buff.py'
large_buff_py = vc_globals.test_data + os.sep + 'large_buff.py'
unusual_symbols_py = vc_globals.test_data + os.sep + 'unusual_symbols.py'

# use this only for foreground tests:
foreground_py = vc_globals.test_data + os.sep + 'foreground.py'
VoiceCodeRootTest.foreground_py = foreground_py

# function for faster definition of new unittest modules

def add_unittest(name, desc, foreground=0):
    """one way call to the add_test function

    assume name + Test == test case module and class
    so name+"Test".name_"Test" can be imported.
    
    """
    test_name = name
    mod_name =  name + "Test"
    try:
        module = __import__(name+"Test")
    except ImportError:
        print '****cannot import test module, skipping test: %s'% mod_name
        return
   
    test_class_name = mod_name
    try:
        test_class = getattr(module, test_class_name)
    except AttributeError:
        print '****cannot find test class in test module, skipping test: %s'% test_class_name
        return

   
    def test_function():
        return unittest.TextTestRunner(). \
                run(unittest.makeSuite(test_class, 'test')) 

    add_test(test_name, test_function, desc=desc, foreground=foreground)



##############################################################################
# Testing the test harness
##############################################################################


add_unittest('TestCaseWithHelpers', desc='Test the unit testing framework.')

add_unittest('DeliberateOutput',
             desc='Testing the output that failures and errors of unittesting'
                  'should give in the output file when run from the mediator test')


##############################################################################
# Testing SymDict
##############################################################################


def accept_symbol_match_test(interp, source, symbol_matches):
    """Does a test on SymDict.accept_symbol_match.
    """
    print '\n\n*** Accept symbol match test. source=\'%s\' ***' \
        % util.within_VCode(source)
    interp.cleanup_dictionary()
    interp.parse_symbols_from_file(source)
    print 'Parsed symbols are: '
    interp.print_symbols()
    print '\n\nUnresolved abbreviations are:'
    unresolved = interp.peek_at_unresolved()
    sorted_unresolved = unresolved.keys()
    sorted_unresolved.sort()
    for an_abbreviation in sorted_unresolved:
        symbol_list = unresolved[an_abbreviation].keys()
        print '\'%s\': appears in %s' % (an_abbreviation, str(symbol_list))

    sys.stdout.write('\n\nAccepting: ')
    for a_match in symbol_matches:
       sys.stdout.write('\'%s\' -> \'%s\', ' % (a_match.pseudo_symbol, a_match.native_symbol))
       interp.accept_symbol_match(a_match)
    sys.stdout.write('\n')


    print '\n\nAfter accepting those symbols, known symbols are:\n'
    interp.print_symbols()
    print '\n\nUnresolved abbreviations are:'
    unresolved = interp.peek_at_unresolved()
    sorted_unresolved = unresolved.keys()
    sorted_unresolved.sort()
    for an_abbreviation in sorted_unresolved:
        symbol_list = unresolved[an_abbreviation].keys()
        print '\'%s\': appears in %s' % (an_abbreviation, str(symbol_list))


    print '\n*** End of accept symbol match test ***\n'


def symbol_match_test(interp, sources, pseudo_symbols):
        """Tests pseudo-symbol matching.

        **INPUTS**

        [MediatorObject] interp -- [MediatorObject] instance to
        use for the test.

        *[STR]* sources -- List of source files to be compiled before
         doing the matches.

        *[STR]* pseudo_symbols -- List of pseudo-symbols to be matched.


        **OUTPUTS**

        *none* --
        """

        strsources = []
        for source in sources:
            strsources.append(util.within_VCode(source))

        print '*** Pseudo symbol match test***\n   Source files are: %s\n   Symbols are: %s\n\n' % (strsources, pseudo_symbols)


        #
        # Compile symbols
        #
        interp.cleanup_dictionary()
        for a_source in sources:
            interp.parse_symbols_from_file(a_source)
#        print '\n Known symbols are: \n'
#        interp.known_symbols.print_symbols()

        #
        # Match the symbols
        #
        for a_symbol in pseudo_symbols:
            matches, weak_matches, forbidden = \
                interp.match_pseudo_symbol(a_symbol)
            sys.stdout.write('\'%s\' matches: [' % a_symbol)
            if matches:
                for a_match in matches:
                    sys.stdout.write('%s, ' % a_match[1])
            else: sys.stdout.write('[]')
            sys.stdout.write(']\n')

        print '\n*** End of Pseudo Symbol Match test ***'



def test_SymDict():
    """Self test for SymDict"""

    global small_buff_c, large_buff_py

    temp_config = temp_factory.new_config()
    interp = temp_config.interpreter()
    test_helpers.compilation_test(interp, small_buff_c)
    test_helpers.compilation_test(interp, large_buff_py)
    pseudo_symbols = ['set attribute', 'expand variables', 'execute file', 'profile Constructor Large Object', 'profile construct large object', 'auto test']
    symbol_match_test(interp, [large_buff_py], pseudo_symbols)

    a_match = SymDict.SymbolMatch(pseudo_symbol='this symbol is unresolved', native_symbol='this_sym_is_unres', words=['this', 'symbol', 'is', 'unresolved'], word_matches=['this', 'sym', 'is', 'unres'])
    accept_symbol_match_test(interp, small_buff_c, [a_match])

    temp_config.quit()

    unittest.TextTestRunner(). \
       run(unittest.makeSuite(SymDictTest.SymDictTest, 'test'))

add_test('SymDict', test_SymDict, desc='self-test for SymDict.py')




##############################################################################
# Testing CmdInterp
##############################################################################

# test_mediator = None


def test_CmdInterp_mediator(temp_config):

    global small_buff_c, small_buff_py, large_buff_py

# I don't think this is necessary (or correct -- we do want the mediator
# to go out of scope) but for regression testing purposes, I'm first
# leaving it in and then will remove it.
#    global test_mediator
#    test_mediator = a_mediator
    a_mediator = temp_config.mediator()
    app = temp_config.editor()
    interp = temp_config.interpreter()
    acmd = CSCmd.CSCmd(spoken_forms=['for', 'for loop'], meanings={contC: c_simple_for, 'python': py_simple_for})
    a_mediator.add_csc(acmd)
    acmd = CSCmd.CSCmd(spoken_forms=['loop body', 'goto body'], meanings={contC: c_goto_body, 'python': py_goto_body})
    a_mediator.add_csc(acmd)
    app.open_file(small_buff_c)
    app.goto(41)
    print '\n\n>>> Testing command interpreter\n\n'
    print '\n>>> Interpreting in a C buffer'
    print '\n>>> Current buffer is:\n'
    app.print_buff()
    old_stdin = util.stdin_read_from_string('1\n')

    #
    # Test if spoken form of CSC is recognised as a single SR vocabulary entry
    # e.g. 'for loop' recognised as: ['for loop\for loop']
    #
    print '>>> Interpreting: %s' % ['for loop', 'loop body']
    interp.interpret_NL_cmd(['for loop', 'loop body'], app)


    #
    # Test if spoken form of CSC is recognised as multiple vocabulary entries
    # e.g. 'for loop' recognised as ['for', 'loop']
    print '>>> Interpreting: %s' % ['for', 'loop', 'loop', 'body']
    interp.interpret_NL_cmd(['for', 'loop', 'loop', 'body'],
        app)
    sys.stdin = old_stdin
    print '\n>>> Buffer is now:'
    app.print_buff()


    app.open_file(small_buff_py)
    app.goto(43)
    app.curr_buffer().language = 'python'
    print '\n>>> Interpreting in a Python buffer'
    print '\n>>> Current buffer is:\n'
    app.print_buff()

    print '>>> Interpreting: %s' % ['for loop', 'loop body']
    interp.interpret_NL_cmd(['for loop', 'loop body'],
        app)
    print '\n>>> Buffer is now:'
    app.print_buff()

    temp_config.quit()

def test_CmdInterp():

    #
    # Create a command interpreter connected to the editor simulator
    #
    temp_config = temp_factory.new_config(skip_config = 1)
#     a_mediator = MediatorObject.MediatorObject(app = EdSim.EdSim(),
#         interp=CmdInterp.CmdInterp())
    test_CmdInterp_mediator(temp_config)

    unittest.TextTestRunner(). \
       run(unittest.makeSuite(CmdInterpTest.CmdInterpTest, 'test'))


add_test('CmdInterp', test_CmdInterp, desc='self-test for CmdInterp.py')

##############################################################################
# Testing SourceBuff
##############################################################################


add_unittest('SourceBuff', desc='Unit tests for the SourceBuff class and subclasses.')



##############################################################################
# Testing context objects
##############################################################################


add_unittest('Context', desc='Tests for conflicting contexts.')

add_unittest('ContBlankLine', desc='Tests for the blank line context class.')

add_unittest('ContPyInsideArguments', desc='Tests for being inside the argument list of a function.')

add_unittest('ContPyBeforeArguments', desc='Tests for being before open paren of a function def or call.')

add_unittest('ContLanguage', desc='Tests for different language contexts.')

##############################################################################
# Testing CSCmd things
##############################################################################

add_unittest('CSCmd', desc='Testing CSCmd')

##############################################################################
# Testing LSAlias definitions
##############################################################################

add_unittest('LSAlias', desc='Testing LSA definitions')


##############################################################################
# Testing WhatCanISay dialogs
##############################################################################

add_unittest('WhatCanISay', desc='self-test for WhatCanISay.py')


##############################################################################
# Testing SymbolResult
##############################################################################

add_unittest('SymbolResult', desc='Testing SymbolResult.')


###############################################################################
# Testing EdSim
###############################################################################

def test_EdSim():
    """Self test for EdSim.py."""

    global small_buff_c, small_buff_py, large_buff_py

    test_buff = small_buff_c
    sim = EdSim.EdSim()
    test_buff2 = posixpath.expandvars('$VCODE_HOME' + os.sep + 'Data' + os.sep + 'TestData' + os.sep + 'small_buff2.c')


    print ">>> Testing EdSim.py"
    print "\n\n>>> Opening a buffer"
    sim.open_file(test_buff)
    sim.print_buff()

    print "\n\n>>> Moving to position 5"
    sim.goto(5)
    sim.print_buff()

    print "\n\n>>> Testing breadcrumbs"
    print "\n>>> Dropping one here"; sim.print_buff()
    sim.drop_breadcrumb()
    sim.goto(10)
    sim.drop_breadcrumb()
    print "\n>>> Dropping one here"; sim.print_buff()
    print "\n>>> Popping 2 crumbs -> end up here:"
    sim.pop_breadcrumbs(num=2)
    sim.print_buff()
    print "\n>>> Dropping one here"; sim.print_buff()
    sim.drop_breadcrumb()
    sim.goto(10)
    print "\n>>> Dropping one here"; sim.print_buff()
    sim.drop_breadcrumb()
    sim.goto(20)
    sim.print_buff()
    sim.pop_breadcrumbs()
    print "\n>>> Popping 1 crumb -> end up here..."
    sim.print_buff()

    print '\n\n>>> Testing code indentation. Inserting for loop.'
    sim.goto(42)
    sim.insert_indent('for (ii=0; ii <= maxValue; ii++)\n{\n', '\n}\n')
    sim.print_buff()
    sim.cleanup()


add_test('EdSim', test_EdSim, desc='self-test for EdSim.py')


###############################################################################
# Testing Object.py
###############################################################################

class Person1(Object.Object):
    def __init__(self, name, citizenship=None, **args_super):
        self.deep_construct(Person1, {'name': name, 'citizenship': citizenship}, args_super)

class Employee1(Person1):
    def __init__(self, salary=None, **args_super):
        self.deep_construct(Employee1, {'salary': salary}, args_super)

class MyPerson(Person1):
    def __init__(self, marital_status=None, **args_super):
        self.deep_construct(MyPerson, {'marital_status': marital_status}, args_super, new_default={'citizenship': 'Canadian eh?'})

class Canadian(Person1):
    def __init__(self, **args_super):
        self.deep_construct(Canadian, {}, args_super, enforce_value={'citizenship': 'Canadian eh?'})

class Person2(Object.Object):
    def __init__(self, name=None, citizenship=None, init_file=None, **args_super):
        self.deep_construct(Person2, {'name': name, 'citizenship': citizenship}, args_super)
        sys.stdout.write('\nPerson2.__init__ received init_file=%s' % init_file)

class AnimatedCharacter:
    def __init__(self, animation_file, frames_per_sec=40):
        self.animation_file = animation_file
        self.frames_per_sec = frames_per_sec


class AnimatedPerson(Person1, AnimatedCharacter):
    def __init__(self, animation_file, frames_per_sec=40, **args_super):
        self.deep_construct(AnimatedPerson, {'animation_file': animation_file, 'frames_per_sec': frames_per_sec}, args_super, exclude_bases={AnimatedCharacter: 1})
        AnimatedCharacter.__init__(self, animation_file, frames_per_sec=frames_per_sec)

def try_attribute(obj, name, operation):
    """Test setting/getting attributes

    **INPUTS**

    *ANY* obj -- object on which we will get/set attributes

    *STR* name -- name of attribute to get/set

    *STR* operation -- *'get'* or *'set'*

    **OUTPUTS**

    *none* --
    """
    env_py_debug_object = None
    if os.environ.has_key('PY_DEBUG_OBJECT'): env_py_debug_object =  os.environ['PY_DEBUG_OBJECT']
    sys.stdout.write("\nTrying to %s the value of attribute '%s', $PY_DEBUG_OBJECT=%s\n   -> " % (operation, name, env_py_debug_object))
    if (operation == 'set'):
        code = "obj." + name + " = '999'"
    else:
        code = "x = obj." + name
    x = 0
    try:
        exec(code)
    except AttributeError, exc:
        sys.stdout.write("Caught AttributeError exception: '%s'" % [exc.__dict__])
    else:
        sys.stdout.write("Caught NO AttributeError exception. ")
        str = "obj.%s=%s, x=%s" % (name, obj.name, x)
        sys.stdout.write(str)
    sys.stdout.write("\n\n")


def test_Object():

    obj = Object.SmallObject()
    sys.stdout.write("Testing exceptions for get/set\n\n")
    try_attribute(obj, 'name', 'get')
    try_attribute(obj, 'name', 'set')
    try_attribute(obj, 'nonexistant', 'get')
    try_attribute(obj, 'nonexistant', 'set')

    result = Employee1(name='Alain', salary='not enough')
    print "Testing inheritance of constructor arguments\n   Employee1(name='Alain', salary='not enough') -> %s\n" % result.__dict__

    print "\nRedefining default value of *citizenship*\n   MyPerson(name='Alain') -> result=%s" % result.__dict__

    result = MyPerson(name='Alain', citizenship='US citizen')
    print "\nOverriding redefined default value of *citizenship*\n   MyPerson(name='Alain', citizenship='US citizen') -> result=%s" % result.__dict__

    result = Canadian(name='Alain')
    print "\nEnforcing 'Canadian eh?' as the value of *citizenship*\n   Canadian(name='Alain') -> result=%s" % result.__dict__

    sys.stdout.write("\nTrying to change enforced value 'Canadian eh?' of *citizenship*\n   Canadian(citizenship='US') -> ")
    try:
        result = Canadian(citizenship='US')
        print 'Test failed. EnforcedConstrArg exception should have been raised but wasn\'t'
    except Object.EnforcedConstrArg, mess:
        print 'Test OK. EnforcedConstrArg was correctly raised: \'%s\'' % mess


    result = Person2(init_file='C:/temp.txt')
    print "\nClass with private *init_file* attribute*\n   Person2(init_file='C:/temp.txt') -> result=%s" % result.__dict__

    result = AnimatedPerson(name='Alain', animation_file='C:/People/Alain.dat')
    print "\nSubclassing from non-standard class AnimatedCharacter.*\n   AnimatedPerson(name='Alain', animation_file='C:/People/Alain.dat') -> result=%s" % result.__dict__


add_test('Object', test_Object, desc='self-test for Object.py')

###############################################################################
# Testing mediator.py console
###############################################################################

def substitute_VCODE_HOME_in_command(command):
    vcode_home_regex = re.sub('[\\\\/]+', '[\\\\\\/]', vc_globals.home)
    command_with_VCODE_HOME_substitution = re.sub(vcode_home_regex, '%VCODE_HOME%',                                                   command, re.IGNORECASE)
    return command_with_VCODE_HOME_substitution


def test_command(command):

    #
    # Substitute %VCODE_HOME% directory in arguments to the command, so that they
    # don't appear in the test output. Otherwise, the test output will differ
    # depending on where the user installed VoiceCode
    #
    command_with_VCODE_HOME_substituted = substitute_VCODE_HOME_in_command(command)

    print '\n\n>>> Testing console command: %s\n' % command_with_VCODE_HOME_substituted
    sys.stdout.flush()
    testing.execute_command(command)
    sys.stdout.flush()

def test_say(utterance, user_input=None, never_bypass_sr_recog=0):
    print '\n\n>>> Testing console command: say(%s, user_input=\'%s\')' % (utterance, user_input)
    sys.stdout.flush()
    commands.say(utterance, user_input, never_bypass_sr_recog=never_bypass_sr_recog)
    sys.stdout.flush()


def test_mediator_console():
    global small_buff_c, small_buff_py, large_buff_py

    testing.init_simulator_regression()
    test_command("""clear_symbols()    """)
    test_command("""open_file('blah.c')""")
    file = small_buff_c
    commands.print_abbreviations()
    test_command("""compile_symbols([r'""" + file + """'])""")
    test_say(['for', 'loop', 'horiz_pos\\horizontal position', 'equals', '0\\zero', 'loop', 'body'])

    test_command("say(['select', 'horiz_pos\\horizontal position'," + \
        " '=\equals'],  never_bypass_sr_recog=1)")
    test_command("""quit(save_speech_files=0, disconnect=0)""")



add_test('mediator_console', test_mediator_console, desc='testing mediator console commands')

###############################################################################
# Testing Select Pseudocode console
###############################################################################


def test_select_pseudocode():

    testing.init_simulator_regression()
    test_command("""open_file('blah.py')""")
    test_say(['index', 'equals', '0\\zero', 'new statement'], user_input='1\\n', never_bypass_sr_recog=1)
    test_say(['index', 'equals', '1\\one', 'new statement'], user_input='1\\n', never_bypass_sr_recog=1)
    test_say(['index', 'equals', '0\\zero', 'new statement'], user_input='1\\n', never_bypass_sr_recog=1)
    test_say(['index', 'equals', '1\\one', 'new statement'], user_input='1\\n', never_bypass_sr_recog=1)
    test_say(['index', 'equals', '0\\zero', 'new statement'], user_input='1\\n', never_bypass_sr_recog=1)

#    util.request_console_be(active=1)

    #
    # Testing go commands
    #
    print "\nTesting 'go' commands...\n"

#    test_command("""goto_line(2)""")
#    test_say(['go', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_command("""goto_line(2)""")
    test_say(['go after next', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_command("""goto_line(2)""")
    test_say(['go after previous', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_command("""goto_line(2)""")
    test_say(['go before', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_command("""goto_line(2)""")
    test_say(['go before next', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_command("""goto_line(2)""")
    test_say(['go before previous', 'index', '=\\equals', '0\\zero'],
        never_bypass_sr_recog=1)
#    test_command("""goto_line(2)""")
#    test_say(['go next', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
#    test_command("""goto_line(2)""")
#    test_say(['go previous', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_command("""goto_line(2)""")
    test_say(['after next', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_command("""goto_line(2)""")
    test_say(['after previous', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_command("""goto_line(2)""")
    test_say(['before', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_command("""goto_line(2)""")
    test_say(['before next', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_command("""goto_line(2)""")
    test_say(['before previous', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_command("""goto_line(2)""")

    #
    # Testing selection commands
    #

    print "\nTesting selection commands...\n"

    test_command("""goto_line(2)""")
    test_say(['select', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_command("""goto_line(2)""")
    test_say(['select next', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_command("""goto_line(2)""")
    test_say(['select previous', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)



    #
    # Testing repeated selection in both directions
    #
    print "\nTesting repeated selection commands in both directions...\n"

    test_command("""goto_line(1)""")
    test_say(['select next', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_say(['select next', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_command("""goto_line(6)""")
    test_say(['select previous', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_say(['select previous', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)


    #
    # Testing repeatability of SelectPseudoCode commands
    #

    print "\nTesting repeatability of SelectPseudoCode commands...\n"
    test_command("""goto_line(1)""")
    test_say(['select', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_say(['next', 'one'])
    test_say(['previous', 'one'])
    test_say(['go after next', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_say(['next', 'one'])
    test_say(['previous', 'one'])

    test_command("""goto_line(1)""")
    test_say(['go before', 'index', '=\\equals', '1\\one'], never_bypass_sr_recog=1)
    test_say(['next', 'one'])
    test_say(['previous', 'one'])

    test_command("""goto_line(1)""")
    test_say(['go after', 'index', '=\\equals', '1\\one'], never_bypass_sr_recog=1)
    test_say(['next', 'one'])
    test_say(['previous', 'one'])

    print "\n   Testing reapeating without specifying direction...\n"
    test_command("""goto_line(1)""")
    test_say(['go after', 'index', '=\\equals', '1\\one'], never_bypass_sr_recog=1)
    test_say(['again'])
    test_command("""goto_line(5)""")
    test_say(['go after', 'index', '=\\equals', '1\\one'], never_bypass_sr_recog=1)
    test_say(['again'])


    # Testing select X through Z
    # AD: There used to be a bug where Select X through Z failed if
    #     if cursor was right before an occurence of X through Z.
    print "\nTesting select X through Z...\n"
    test_command("""goto_line(1)""")
    test_say(['select', 'index', 'through', '1\\one'])
    test_say(['select', 'index', 'through', '0\\zero'])

    test_command("""quit(save_speech_files=0, disconnect=0)""")


add_test('select_pseudocode', test_select_pseudocode, desc='testing select pseudocode commands')


###############################################################################
# Testing Oddities in Select Pseudocode With Natspeak 7
###############################################################################


def test_v7_select():
    testing.init_simulator_regression()
    test_command("""open_file('blah.py')""")
    test_say(['index', 'equals', '0\\zero', 'new statement'], user_input='1\\n', never_bypass_sr_recog=1)
    test_say(['index', 'equals', '1\\one', 'new statement'], user_input='1\\n', never_bypass_sr_recog=1)
    test_say(['index', '=\\equals', '0\\zero', 'new statement'], user_input='1\\n', never_bypass_sr_recog=1)
    test_say(['index', '=\\equals', '1\\one', 'new statement'], user_input='1\\n', never_bypass_sr_recog=1)

    # Testing selectionn commands
    #
    test_command("""goto_line(2)""")
    test_say(['select', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_command("""goto_line(2)""")
    test_say(['select next', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)
    test_command("""goto_line(2)""")
    test_say(['select previous', 'index', '=\\equals', '0\\zero'], never_bypass_sr_recog=1)


add_test('v7_select', test_v7_select, desc='testing oddities in select pseudocode with Natspeak 7')



##############################################################################
# Testing automatic addition of abbreviations
##############################################################################

def test_auto_add_abbrevs():

    global small_buff_c, small_buff_py, large_buff_py

    testing.init_simulator_regression()

    test_command("""open_file('blah.c')""")
    print repr(vc_globals.test_data)
    file = small_buff_c
    test_command("""compile_symbols([r'""" + file + """'])""")
    test_command("""print_abbreviations(1)""")

    #
    # Match selection dialog should be invoked, and abbreviation
    # unres->unresolved should be added
    #
# Actually, the match selection dialog no longer exists, and dictation
# doesn't have the effect of confirming a match and resolving
# abbreviations, so we have to do this manually
    test_say(['this', 'symbol', 'is', 'unresolved', ',\\comma'], user_input='1\\n')
    test_command("""print_abbreviations(1)""")
    test_command("""print_symbols()""")
    testing.mediator().interpreter().known_symbols.add_extra_expansion('unres',
        'unresolved')


    #
    # Match selection dialog should NOT be invoked
    #
    test_say(['this_sym_is_unres_too\\this symbol is unresolved too', ',\\comma'])
    test_command("""print_symbols()""")
    test_command("""print_abbreviations(1)""")

    #
    # Match selection dialog should be invoked, and abbreviation
    # f->file should NOT be added (too short)
    #
    test_say(['file', 'name', ',\\comma'], user_input='1\\n')
    test_command("""print_symbols()""")
    test_command("""print_abbreviations(1)""")

    #
    # Case with abbreviations which are the first letter of every word
    # (API->"Application Programming Interface"). Should be added as
    # a single abbreviation instead of three separate ones (A->applicaiton,
    # P->programming, I->interface).
    #
    test_say(['application', 'programming', 'interface', 'function', ',\\comma'], user_input='1\\n')
    test_command("""print_abbreviations(1)""")
    test_command("""quit(save_speech_files=0, disconnect=0)""")

add_test('automatic_abbreviations', test_auto_add_abbrevs, desc='testing automatic creation of abbreviations')


##############################################################################
# Testing compilation and dictation of unusual symbols
##############################################################################


# Based on number_dictation
def test_number_dictation_COPY():
   testing.init_simulator_regression()
   commands.open_file('blah.py')
   commands.say(['23\\twenty-three', '54\\fifty-four', 'comma', '0\\zero', '.\\point', '04\\oh four'], echo_cmd=1)
   commands.say(['select', '0\\zero', '.\\point'], never_bypass_sr_recog=1, echo_cmd=1, user_input="0\n")
   commands.say(['select', '0\\zero', '.\\point'], never_bypass_sr_recog=1, echo_cmd=1, user_input="0\n")
   commands.say(['select', '04\\oh four'], never_bypass_sr_recog=1, echo_cmd=1, user_input="0\n")
   commands.say(['select', '0\\zero', '.\\point', 'oh', 'four'], never_bypass_sr_recog=1, echo_cmd=1, user_input="0\n")


def test_unusual_symbols():

   global unusual_symbols_py

   testing.init_simulator_regression()
   test_helpers.compilation_test(testing.mediator().interpreter(), unusual_symbols_py)

   commands.open_file('blah.py')

   print "Next utterance should not match short symbol 'se'"
   commands.say(['software', 'engineering', 'comma'], echo_cmd=1, user_input="1\n")

   print "Next utterance should match non-separable symbol 'openlog'"
   commands.say(['open', 'log'], echo_cmd=1, user_input="1\n")

#   print "Trying to select 'openlog'"
#   try:
#      commands.say(['select', 'openlog\\open log'], echo_cmd=1, never_bypass_sr=1)
#   except:
#      print "Select of 'openlog' failed."



add_test('unusual_symbols', test_unusual_symbols, desc='compilation and dictation of unusual symbols')


##############################################################################
# Testing persistence between VoiceCode sessions
##############################################################################

def test_persistence():

    global small_buff_c, small_buff_py, large_buff_py

    print """As best as I can tell from SF Browse CVS, this test was
    introduced in revision vcode vcode-0-0-7, and broken before
    vcode-0-0-8, and has never since worked in the manner the
    printed >>> comments indicate, until now -- DCF"""
    #
    # Create make mediator console use an empty file for SymDict persistence
    #
    fname = vc_globals.tmp + os.sep + 'tmp_symdict.dict'
    try:
        os.remove(fname)
    except:
        # Never mind if file doesn't exist
        pass

    print '\n\n>>> Starting mediator with persistence'
# it makes more sense to use TempConfig here
#
# ugh - we want abbreviations defined (so we can't use skip_config)
# but we don't want standard symbols defined.  How can
# we achieve that?  Oddly enough, configuration doesn't seem to define any
# standard symbols - why not?
#
# Question... could this be refactored to get the interp from testing.interp()
# instead? Trying to get rid of the temp_factory. AD.
#
# Answer: No.  The persistence test needs to use its own interpreter,
# so that it doesn't interfere with the other tests.
#

#    temp_config = temp_factory.new_config(skip_config = 1)
# create new interpreter without a pickle file, but do configure it
    temp_config = temp_factory.new_config()
    interp = temp_config.interpreter()

    #
    # Compile symbols
    #
    file = small_buff_c
    fake_command = "compile_symbols([r'%s'])" % file
    print '\n\n>>> Testing console command: %s\n' % fake_command
    sys.stdout.flush()
    interp.parse_symbols_from_files([file])
    print '>>> Known symbols are: '; interp.print_symbols()
    sys.stdout.flush()
# but manually pickle before quitting (quit would clean out the symbol
# dictionary and reparse the standard files before pickling)
    interp.known_symbols.save(file = fname)

    #
    # Restart the mediator, with saved SymDict. The symbols should still be
    # there
    #
    print '\n\n>>> Restarting mediator with persistence. Compiled symbols should still be in the dictionary.\n'

    fake_command = "quit(save_speech_files=0, disconnect=0)"
    print '\n\n>>> Testing console command: %s\n' % fake_command
# this actually does do something
    temp_config.quit()

# DCF: this doesn't have the desired effect.  Currently, with
# PersistentConfig, it does a reset, but NMO uses the SymDict pickled
# to a string via pickle.dumps for regression testing.  If we switch
# to TempConfig and use new_config (which we probably should), we will
# still have to  modify new_config to allow for
#
# (1) the ability to bypass the dumps-pickled SymDict and use the
# specified one.
# (2) the ability to bypass the dumps-pickled SymDict, and create
# SymDict from scratch (but not load the standard files during
# configuration, which would be the normal thing to do in the new
# scheme).  Or should we skip configuration?

# now, start with the file we created
    temp_config = temp_factory.new_config(skip_config = 1,
        alt_sym_file = fname)
    interp = temp_config.interpreter()
    fake_command = "print_symbols()"
    print '\n\n>>> Testing console command: %s\n' % fake_command
    interp.print_symbols()

    #
    # Restart the mediator without saved SymDict. The symbols should not be
    # there anymore.
    #
    print '\n\n>>> Restarting mediator WITHOUT persistence. There should be NO symbols in the dictionary.\n'
    fake_command = "quit(save_speech_files=0, disconnect=0)"
    print '\n\n>>> Testing console command: %s\n' % fake_command
    temp_config.quit()
# back to no file
    temp_config = temp_factory.new_config(skip_config = 1)
    interp = temp_config.interpreter()

    fake_command = "print_symbols()"
    print '\n\n>>> Testing console command: %s\n' % fake_command
    interp.print_symbols()

    fake_command = "quit(save_speech_files=0, disconnect=0)"
    print '\n\n>>> Testing console command: %s\n' % fake_command
    temp_config.quit()


add_test('persistence', test_persistence, desc='testing persistence between VoiceCode sessions')


##############################################################################
# Testing persistent SymDict storage system, especially the version
# updating system
##############################################################################

class DictReadFailure:
    def __init__(self):
        self.reset()
    def reset(self):
        self.failed = 0
        self.msg = ''
    def on_failure(self, message):
        self.failed = 1
        self.msg = message

class InterceptInputError:
    def __init__(self):
        self.reset()
    def reset(self):
        self.error = 0
        self.msg = ''
        self.fatal = 0
    def input_error(self, msg, fatal = 0):
        self.error = 1
        self.msg = msg
        self.fatal = fatal

def mod_diff(x, y):
    raw_diff = difflib.ndiff(x, y)
    return filter(lambda x: x[0] != ' ', raw_diff)

fake_words = {
              'ADPCM': 0,
              'BOM': 0,
              'RST': 0,
              'SND': 0,
              'HLS': 0,
              'NEG': 0,
              'TZ': 0,
              'MX': 0,
              'O': 0,
              'ST': 0,
              'CMP': 0,
              ('multi', 'multi-'): 1,
              ('co', 'co-'): 1,
              'CRC': 0,
              'HSV': 0,
              'IM': 0,
              'NI': 0,
              'TTY': 1,
              'UU': 0
              }
#fake_words = {}

def word_exists(spoken_form, written_form = None):
    if written_form is None:
        entry = spoken_form
    else:
        entry = (spoken_form, written_form)
    try:
        return fake_words[entry]
    except KeyError:
        entry = sr_interface.vocabulary_entry(spoken_form, written_form)
        return not (sr_interface.getWordInfo(entry) is None)

def test_SymDict_storage():
    test_data = vc_globals.test_data
    export_file = os.path.join(test_data, 'abbrevs')

# create a new SymDict, without reading from a file
    symbols = SymDict.SymDict(export_file = export_file)
    symbols.export_abbreviations(export_file + '.py')
# this allows us to get consistent results with different versions of
# Natspeak with different built-in vocabularies
    symbols.word_exists = word_exists

    f = open(export_file + '.py', "r")
    print "Exporting abbreviations from an empty SymDict, so"
    print "the following export file should be empty (except for"
    print "one blank line)"
    print "-----------------------------------------"
    line = f.readline()
    last = '\n'
    while line:
        sys.stdout.write(line)
        last = line
        line = f.readline()
    f.close()
    if last[-1] != '\n': print '\n'
    print "-----------------------------------------\n"

    symbol_file = os.path.join(test_data, 'selected_py_sym.py')
    symbols.standard_symbols_in([symbol_file])

    C_Cpp_abbrev_file = os.path.join(test_data, 'C_Cpp_abbrevs.py')
    py_abbrev_file = os.path.join(test_data, 'py_abbrevs.py')
    std_abbrev_file = os.path.join(test_data, 'std_abbrevs.py')
    abbrev_files = [std_abbrev_file, py_abbrev_file, C_Cpp_abbrev_file]
#    abbrev_files = [py_abbrev_file]
    symbols.abbreviations_in(abbrev_files)

# make sure we read those symbol and abbreviation files
    symbols.finish_config(mark_user = 0)

    dict_file = os.path.join(test_data, 'symdict.dict')
    mod_dict_file = os.path.join(test_data, 'modified_symdict.dict')

    symbols.sym_file = dict_file
    symbols.save(dict_file)

    fail = DictReadFailure()
    persistent_symbols = SymDict.SymDict(export_file = export_file + '2')
    persistent_symbols.sym_file = dict_file
    persistent_symbols.word_exists = word_exists
    d = persistent_symbols.dict_from_file(on_failure = fail.on_failure)
    if fail.failed:
        print "ERROR: failed to read \n%s:" % util.within_VCode(dict_file)
        print fail.msg
        return

    version = d['version']
    if version != SymDict.current_version:
        print "ERROR: version in persistent file doesn't match"
        print "SymDict.current_version"
        return

    symbols.prepend_abbreviations('color', ['clr'])
    symbols.add_abbreviation('visual', 'vis')
    print "\nsaving modified SymDict"
    symbols.save(mod_dict_file)
    d_mod = symbols.persistent_dict()

    print "attempting to recover symbols and abbreviations with"
    print "bad persistent SymDict"

    junk_file = os.path.join(test_data, 'bad.dict')
    f = open(junk_file, "w")
    f.write('This file is not actually a persistent SymDict file\n')
    f.close()

    ierr = InterceptInputError()
    recovered_symbols = SymDict.SymDict(interp = ierr, sym_file = junk_file,
        export_file = export_file)
    recovered_symbols.word_exists = word_exists
    if not ierr.error:
        print "ERROR: SymDict did not report an error on init from"
        print "a bad persistent SymDict"
        return
    if ierr.fatal:
        print "ERROR: SymDict reported a fatal error on init from"
        print "a bad persistent SymDict, indicating that it failed"
        print "to recover abbreviation preferences from the text"
        print "file of exported abbreviations"
        return

# only abbreviation preferences will have been recovered from the text
# file, so we need to recover known symbols from the standard files

    print "finishing configuration of the recovered SymDict"

    recovered_symbols.standard_symbols_in([symbol_file])
    recovered_symbols.abbreviations_in(abbrev_files)
    recovered_symbols.finish_config(mark_user = 0)

    d_recovered = recovered_symbols.persistent_dict()

    print "comparing modified persistent dictionary with the recovered one"

# the two dictionaries should also be the same in detail
    o = DiffCrawler.ObjDiff(all = 1)
    mod_sources = d_mod['symbol_sources_read']
    for i in range(len(mod_sources)):
        mod_sources[i] = os.path.basename(mod_sources[i])
    recovered_sources = d_recovered['symbol_sources_read']
    for i in range(len(recovered_sources)):
        recovered_sources[i] = os.path.basename(recovered_sources[i])

    o.compare(d_mod, d_recovered)
    same_difference = o.differences()
    if same_difference:
        print "ERROR: Differences found:"
        for diff in same_difference:
            print diff.location()
            print diff.description()
            print ""
        print "\nFix problems with recovery from exported abbreviation"
        print "file, and re-run this test"
        return

    print "no differences found\n"





    curr_dict_file = os.path.join(test_data, 'current_symdict.dict')
    prev_dict_file = os.path.join(test_data, 'previous_symdict.dict')
    curr_symbols = SymDict.SymDict(export_file = export_file + '.current')
    curr_symbols.sym_file = curr_dict_file
    curr_symbols.word_exists = word_exists
    d_curr = curr_symbols.dict_from_file(on_failure = fail.on_failure)
    if fail.failed:
        print "ERROR: failed to read \n%s:" % util.within_VCode(curr_dict_file)
        print fail.msg
        return

    was_version = d_curr['version']
    if was_version == version:
        print "version number unchanged\n"
        print "comparing symdict.dict created from scratch with"
        print "the reference dictionary current_symdict.dict"
        s = DiffCrawler.StructDiff(ignore_heterogeneous = 1, all = 1)
# since the two dictionaries should be the same in detail, we ignore
# heterogeneous containers (but not empty ones) for now, assuming that
# the detailed comparison with ObjDiff will catch any problems in such
# containers
        curr_sources = d_curr['symbol_sources_read']
        for i in range(len(curr_sources)):
            curr_sources[i] = os.path.basename(curr_sources[i])
        sources = d['symbol_sources_read']
        for i in range(len(sources)):
            sources[i] = os.path.basename(sources[i])
        s.compare(d_curr, d)
        same_difference = s.differences()
        incr_version = \
            "\nIf these differences do reflect changes made to\n" \
          + "the persistent SymDict format, you must:\n" \
          + "  1. increment current_version in SymDict.py\n" \
          + "  2. create a new SingleVersionDictConverter to\n" \
          + "     convert from the old version\n" \
          + "  3. prepend this converter to the global symdict_cvtr\n" \
          + "     in SymDict.py\n" \
          + "  4. re-run this test to verify that the old version\n" \
          + "     can be properly converted\n"
        err = \
            "ERROR: %s of persistent SymDict dictionary appears\n" \
          + "to have changed, without a change in the version number.\n" \
          + "Both current_symdict.dict and symdict.dict\n" \
          + "(the latter created from scratch) have version %d\n" \
          + "but the following %sdifferences were found:\n"
        if same_difference:
            print err % ('structure', version, 'structural ')
            for diff in same_difference:
                print diff.location()
                print diff.description()
                print ""
            print incr_version
            return

# Currently (8/2/03), we are still adding symbols containing unresolved
# abbreviations to the vocabulary.  As a result, the SymDict object
# symbols, which we create from scratch, will not detect single word
# symbols as unresolved abbreviations, if they are present in the
# vocabulary from a previous test.  This hack avoids erroneously
# reporting this condition as if it were a change in the persistent
# SymDict storage system.
# - DCF
        d['unresolved_abbreviations'] = d_curr['unresolved_abbreviations']

# the two dictionaries should also be the same in detail
        o = DiffCrawler.ObjDiff(all = 1)
        o.compare(d_curr, d)
        same_difference = o.differences()
        if same_difference:
            print err % ('contents', version, '')
            for diff in same_difference:
                print diff.location()
                print diff.description()
                print ""
            print "\nThese differences could be due to a change to"
            print "the persistent SymDict format without a corresponding"
            print "increment in current_version in SymDict.py.  They could"
            print "also be due to changes to the algorithm for generating"
            print "spoken forms for known symbols, or to the input files"
            print "py_abbrevs.py and selected_py_sym.py in"
            print util.within_VCode(vc_globals.test_data)
            print incr_version
            print "\nIf these differences are simply due to changes"
            print "in the input files or the algorithms, then you must"
            print "verify the new stored symdict.dict in "
            print util.within_VCode(vc_globals.test_data)
            print "manually, and then copy it to current_symdict.dict"
            print "in the same directory."
            return
        print "no differences found\n"

        print "initializing SymDict from dictionary\n"
        fail.reset()
        okay = curr_symbols.init_from_dictionary(d_curr,
            on_failure = fail.on_failure)
        if fail.failed:
            print "ERROR: failed to initialize SymDict from dictionary"
            print "read from %s\n" % util.within_VCode(curr_dict_file)
            print fail.msg
            print "\nFix the problem and re-run this test"
            return
        if not okay:
            print "ERROR: init_from_dictionary returned false, but"
            print "on_failure was not called."
            return
        print "initialization from dictionary was successful"
        okay = curr_symbols.export_abbreviations(export_file + '.current.py')
        if not okay:
            print "ERROR: failed to export abbreviations to file"
            return
        curr_file = export_file + '.current.py'
        prev_file = export_file + '.current.bak'
        try:
            f = open(curr_file, "r")
            curr_abbrev = f.readlines()
            f.close()
        except IOError:
            print "ERROR: unable to read abbreviations file %s" % \
                util.within_VCode(curr_file)
            return
        try:
            f = open(prev_file, "r")
            prev_abbrev = f.readlines()
            f.close()
            changes = mod_diff(prev_abbrev, curr_abbrev)
            if changes:
                print "Unexpected differences between %s\n and %s:\n" \
                    % (util.within_VCode(prev_file),
                       util.within_VCode(curr_file))
                sys.stdout.writelines(changes)
                print "\nIf these differences are not significant, simply"
                print "re-run the test to eliminate this error message"
            else:
                print "Abbreviation file was unchanged, as expected"
        except IOError:
            print "ERROR: unable to read previous abbreviations file %s" % \
                util.within_VCode(prev_file)

        print "initializing fresh SymDict from dictionary\n"
        fail.reset()
        okay = persistent_symbols.init_from_dictionary(d,
            on_failure = fail.on_failure)
        if fail.failed:
            print "ERROR: failed to initialize SymDict from dictionary"
            print "read from %s\n" % util.within_VCode(dict_file)
            print fail.msg
            print "\nFix the problem and re-run this test"
            return
        if not okay:
            print "ERROR: init_from_dictionary returned false, but"
            print "on_failure was not called."
            return
        print "initialization from dictionary was successful"
        fresh_file = export_file + '.py'
        okay = persistent_symbols.export_abbreviations(fresh_file)
        if not okay:
            print "ERROR: failed to export abbreviations to file"
            return
        try:
            f = open(fresh_file, "r")
            fresh_abbrev = f.readlines()
            f.close()
            changes = mod_diff(curr_abbrev, fresh_abbrev)
            if changes:
                print "Unexpected differences between %s\n and %s:\n" \
                    % (util.within_VCode(curr_file),
                    util.within_VCode(fresh_abbrev))
                sys.stdout.writelines(changes)
                print "\nThe most likely cause is a change to the input"
                print "files py_abbrevs.py and selected_py_sym.py in"
                print util.within_VCode(vc_globals.test_data)
                print "\nIf these files have been changed, simply copy"
                print "symdict.dict to current_symdict.dict and re-run"
                print "this test."
            else:
                print "Fresh abbreviation file matches current one, as expected"
        except IOError:
            print "ERROR: unable to read abbreviations file %s" % \
                util.within_VCode(fresh_file)
            return

    elif was_version > version:
        print "ERROR: version number decreased from %d to %d" \
            % (was_version, version)
        return

    else:
        if was_version < version - 1:
            print "version skipped from %d to %d" % (was_version, version)
        print "Attempting to convert from version %d" % was_version
        print "to current version, %d" % version
        fail.reset()
        okay = curr_symbols.init_from_file(on_failure = fail.on_failure)
        if fail.failed:
            print "ERROR: failed to convert to current version"
            print fail.msg
            print "\nFix the problem with the conversion and re-run this test"
            return
        if not okay:
            print "ERROR: failed to re-read the dictionary from "
            print util.within_VCode(curr_dict_file)
            return
        print "version update was successful"
        name, ext = os.path.splitext(curr_dict_file)
        backup = name + '.%d' % was_version + ext
        if not os.path.exists(backup):
            print "\nSymDict failed to rename %s\nto %s" % \
                (util.within_VCode(curr_dict_file), util.within_VCode(backup))
            return
# undo the renaming, so that failures in the following several checks will
# cause the next attempt to run this regression test to try the whole
# conversion over again
        os.rename(backup, curr_dict_file)
        curr_file = export_file + '.current.py'
        try:
            f = open(curr_file, "r")
            curr_abbrev = f.readlines()
            f.close()
        except IOError:
            print "ERROR: unable to read abbreviations file %s" % \
                util.within_VCode(curr_file)
            return
        init_file = export_file + '.on_init.py'
        try:
            f = open(init_file, "r")
            init_abbrev = f.readlines()
            f.close()
            changes = mod_diff(curr_abbrev, init_abbrev)
            if changes:
                print "Unexpected differences between %s\n and %s:\n" \
                    % (util.within_VCode(curr_file),
                       util.within_VCode(init_file))
                sys.stdout.writelines(changes)
                print "\nThis may indicate that abbreviation preferences"
                print "were lost during the update"
                print "\nIf these differences are not significant, copy"
                print "replace the former file with a copy of the latter"
                return
            else:
                print "Abbreviation file was unchanged, as expected"
        except IOError:
            print "ERROR: unable to read new abbreviations file %s" % \
                util.within_VCode(init_file)
            return


        print "\nupdate successful"
        if was_version < version - 1:
            print "\nHowever, version skipped from %d to %d" % (was_version, version)
            print "therefore, we are leaving the old version as"
            print util.within_VCode(curr_dict_file)
            print "If the skip was accidental, fix the new version number"
            print "and re-run these tests"
            print "If the skip was done deliberately, please manually"
            print "rename this file to %s,\n" % \
                util.within_VCode(prev_dict_file)
            print "rename %s\nto %s" % (util.within_VCode(dict_file),
                util.within_VCode(curr_dict_file))
            print "and re-run this test"
        else:
        # update worked, and abbreviation files compared okay, so now
        # we replace current_symdict.dict with the fresh version
            os.rename(curr_dict_file, prev_dict_file)
            persistent_symbols.save(curr_dict_file)
            print "re-run the test to check the new current version"



add_test('SymDict_storage', test_SymDict_storage,
    desc='testing storage and version updating system for SymDict')



##############################################################################
# Testing redundant translation of LSAs and symbols
##############################################################################

add_unittest('LessThanGreaterThan', 'testing variants of less than and greater than')

def test_redundant_translation():
    global small_buff_c

    testing.init_simulator_regression()
    test_command("""open_file('blah.c')""")
    test_command("""compile_symbols([r'""" + small_buff_c + """'])""")
    test_say(['index', '!=\\not equal to', '0\\zero'], '0\n0\n')
    test_say(['index', 'not', 'equal', 'to', '0\\zero'], '0\n0\n')
    test_say(['move_horiz\\move horizontally'], '0\n0\n')
    test_say(['move', 'horizontally'], '0\n0\n')
    test_command("""quit(save_speech_files=0, disconnect=0)""")

add_test('redundant_translation', test_redundant_translation, desc='testing redundant translation of LSAs and symbols at SR and Mediator level')


##############################################################################
# Testing dictation and navigation of punctuation
##############################################################################

def test_punctuation():
    testing.init_simulator_regression()
    commands.open_file('blah.py')

    commands.say(['variable', '\\blank space', '=\\equals', '\\space-bar', 'index', '*\\asterisk', '2\\two', '**\\double asterisk', '8\\eight', '\\New-Line'], user_input='1\n2\n1\n1\n1\n1\n1\n', echo_utterance=1)

## causes recognitionMimic error in Natspeak 4
#    commands.say(['variable', '=\\equals', 'variable', '/\\slash', '2\\two', '+\\plus-sign', '1\\one', '-\\minus-sign', 'index', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['variable', 'equals', 'variable', '/\\slash', '2\\two', '+\\plus-sign', '1\\one', '-\\minus-sign', 'index', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['variable', '=\\equals', 'index', '%\\percent', '2\\two', '+\\plus', 'index', '%\\percent-sign', '3\\three', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['if', 'index', '&\\ampersand', 'variable', 'then'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['if', 'index', '|\\pipe', 'variable', '|\\pipe-sign', 'index', '|\\vertical-bar', 'value', 'then'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['index', '=\\equals', '0\\zero', ';\\semicolon', 'variable', '=\\equals', '0\\zero', ';\\semi', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['index', '.\\dot', 'function', '()\\without arguments', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['variable', '=\\equals', 'new', 'list', '0\\zero', '...\\ellipsis', '10\\ten', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['#\\number-sign', '!\\bang', 'python', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['#\\number-sign', '!\\exclamation-mark', 'python', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['if', '~\\tilde', 'index', 'and', '~\\squiggle', 'variable', 'then'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['variable', '::\\double colon', 'index', '::\\colon colon', 'field', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['if', 'index', '<\\less-than', '0\\zero', 'and\\and', 'index', '>\\greater-than', '-\\minus-sign', '1\\one', 'then'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['index', '=\\equal-sign', '0\\zero', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['function', '(\\open-paren', '0\\zero', ')\\close-paren', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['function', 'parens', '0\\zero', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['function', '()\\empty parens', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['list', '[\\open-square-bracket', '0\\zero', ']\\close-square-bracket', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['list', 'brackets', '0\\zero', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['list', '[]\\empty brackets', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)


## BUG: causes recognitionMimic error
##    commands.say(['dictionary', '{\\open-brace', '0\\zero', '}\\close-brace', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['dictionary', 'braces', '0\\zero', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

## BUG: causes recognitionMimic error
##    commands.say(['dictionary', '{}\\empty braces', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['<\\open-angle', 'head', '>\\close-angle', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['angle brackets', 'head', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['<>\\empty angles', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['pattern', 'equals', 'raw', 'string', 'single', 'quotes',
        'back slash sierra', 'asterisk', 'back slash cap sierra',
        '+\\plus-sign', 'new statement'], echo_utterance = 1)

    commands.say(['string', '=\\equals', '\'\\open-single-quote', 'message', '\'\\close-single-quote', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

## causes recognitionMimic error in Natspeak 4
#    commands.say(['string', '=\\equals', 'single', 'quotes', 'message', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['string', 'equals', 'single', 'quotes', 'message', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)


    commands.say(['\'\'\\empty single-quotes', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['string', '=\\equals', '\"\\open-quote', 'message', '\"\\close-quote', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

## causes recognitionMimic error in Natspeak 4
#    commands.say(['string', '=\\equals', 'quotes', 'message', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['string', 'equals', 'quotes', 'message', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['""\\empty quotes', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['string', '=\\equals', '`\\open-backquote', 'message', '`\\close-backquote', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['string', '=\\equals', 'backquotes', 'message', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['``\\empty backquotes', 'new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    letters = alpha_bravo.keys()
    letters.sort()
    for letter in letters:
        commands.say(['quotes', 'back slash %s.' % letter.upper(),
            'back slash %s' % alpha_bravo[letter], 'new statement'],
            echo_utterance = 1)
    for letter in letters:
        commands.say(['quotes', 'back slash cap %s.' % letter.upper(),
            'back slash cap %s' % alpha_bravo[letter], 'new statement'],
            echo_utterance = 1)

    commands.say(['index', 'semi', 'variable', 'semi'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['previous semi', 'previous semi'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)


    commands.say(['after semi'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before previous semi'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after semi'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before semi'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['variable', '=\\equals', 'brackets', '0\\zero', ',\\comma', '1\\one', ',\\comma', '3\\three'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['previous comma'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after comma'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous comma'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next comma'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['variable', '.\\dot', 'field', '.\\dot', 'value'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['previous dot', 'previous dot'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after dot'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous dot'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next dot'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['braces', 'variable', ':\\colon', '0\\zero', 'comma', 'value', ':\\colon', '0\\zero'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['previous colon', 'previous colon'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after colon'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous colon'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next colon'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['variable', '=\\equals', '2\\two', '*\\asterisk', '3\\three', '*\\asterisk', '4\\four'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['previous asterisk', 'previous star'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after star'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous asterisk'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next star'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

## causes recognitionMimic error in Natspeak 4
#    commands.say(['variable', '=\\equals', '2\\two', '/\\slash', '3\\three', '/\\slash', '4\\four'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['variable', 'equals', '2\\two', '/\\slash', '3\\three', '/\\slash', '4\\four'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['previous slash', 'previous slash'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after slash'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous slash'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next slash'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['variable', '=\\equals', '2\\two', '+\\plus', '3\\three', '+\\plus', '4\\four'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['previous plus', 'previous plus'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after plus'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous plus'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next plus'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

## causes recognitionMimic error in Natspeak 4
#    commands.say(['variable', '=\\equals', '2\\two', '-\\minus', '3\\three', '-\\minus', '4\\four'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['variable', 'equals', '2\\two', '-\\minus', '3\\three', '-\\minus', '4\\four'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['previous minus', 'previous minus'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after minus'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous minus'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next minus'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

## causes recognitionMimic error in Natspeak 4
#    commands.say(['variable', '=\\equals', '2\\two', '%\\modulo', '3\\three', '%\\modulo', '4\\four'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['variable', 'equals', '2\\two', '%\\modulo', '3\\three', '%\\modulo', '4\\four'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['previous percent', 'previous percent'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after percent'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous percent'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next percent'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['0\\zero', '&\\ampersand', '1\\one', '&\\ampersand', '2\\two'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['previous ampersand', 'previous ampersand'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after ampersand'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous ampersand'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next ampersand'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['0\\zero', '|\\pipe', '1\\one', '|\\pipe', '2\\two'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['previous pipe', 'previous pipe'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after pipe'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous pipe'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next pipe'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['0\\zero', '...\\ellipsis', '1\\one', '...\\ellipsis', '2\\two'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['previous ellipsis', 'previous ellipsis'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after ellipsis'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous ellipsis'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next ellipsis'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['0\\zero', '!\\bang', '1\\one', '!\\bang', '2\\two'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['previous bang', 'previous bang'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after bang'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous bang'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next bang'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['0\\zero', '?\\question-mark', '1\\one', '?\\question-mark', '2\\two'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['previous question-mark', 'previous question-mark'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after question-mark'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous question-mark'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next question-mark'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['0\\zero', '#\\number-sign', '1\\one', '#\\number-sign', '2\\two'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['previous number-sign', 'previous number-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after number-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous number-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next number-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['0\\zero', '::\\double colon', '1\\one', '::\\double colon', '2\\two'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['previous double colon', 'previous double colon'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after double colon'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous double colon'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next double colon'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['0\\zero', '~\\tilde', '1\\one', '~\\tilde', '2\\two'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['previous tilde', 'previous tilde'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after tilde'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous tilde'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next tilde'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['0\\zero', '<\\less-than', '1\\one', '<\\less-than', '2\\two'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    #QH, changed less-than to less-than-sign, greater-than to greater-than-sign
    commands.say(['previous less-than-sign', 'previous less-than-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after less-than-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous less-than-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next less-than-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['0\\zero', '>\\greater-than', '1\\one', '>\\greater-than', '2\\two'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['previous greater-than-sign', 'previous greater-than-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after greater-than-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous greater-than-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next greater-than-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['0\\zero', '>\\greater-than-sign', '1\\one', '>\\greater-than-sign', '2\\two'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    # changed greater-than to greater-than-sign:
    commands.say(['previous greater-than-sign', 'previous greater-than-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after greater-than-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous greater-than-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next greater-than-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['0\\zero', '=\\equal-sign', '1\\one', '=\\equal-sign', '2\\two'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['previous equal-sign', 'previous equal-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after equal-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous equal-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next equal-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    # added QH:
    commands.say(['0\\zero', 'equal', '1\\one', 'equal', '2\\two'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['previous equal-sign', 'previous equal-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['after equal-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before previous equal-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['before next equal-sign'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['new statement'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['between parens', '1\\one'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before previous paren'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['after paren'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before paren'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['previous paren'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['out of parens'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before previous paren'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['back out of parens'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['new statement'], user_input='2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['between brackets', '1\\one'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before previous bracket'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['after bracket'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before bracket'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['previous bracket'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['out of brackets'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before previous bracket'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['back out of brackets'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['new statement'], user_input='2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['between braces', '1\\one'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before previous brace'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['after brace'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before brace'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['previous brace'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['out of braces'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before previous brace'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['back out of braces'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['new statement'], user_input='2\n2\n2\n2\n', echo_utterance=1)


    commands.say(['between angles', '1\\one'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before previous angle'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['after angle'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before angle'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['previous angle'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['out of angles'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before previous angle'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['back out of angles'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['new statement'], user_input='2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['between single-quotes', '1\\one'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before previous single-quote'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['after single-quote'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before single-quote'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['previous single-quote'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['out of single-quotes'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before previous single-quote'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['back out of single-quotes'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['new statement'], user_input='2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['between quotes', '1\\one'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before previous quote'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['after quote'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before quote'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['previous quote'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['out of quotes'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before previous quote'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['back out of quotes'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['new statement'], user_input='2\n2\n2\n2\n', echo_utterance=1)

    commands.say(['between backquotes', '1\\one'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before previous backquote'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['after backquote'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before backquote'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['previous backquote'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['out of backquotes'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['before previous backquote'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['back out of backquotes'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['new statement'], user_input='2\n2\n2\n2\n', echo_utterance=1)


    commands.quit(save_speech_files=0, disconnect=0)

add_test('punctuation', test_punctuation, 'testing the various Python CSCs and LSAs')


##############################################################################
# Testing dictation of operators
##############################################################################

def test_operators():
    testing.init_simulator_regression()
    commands.open_file('blah.py')

    commands.say(['count', 'plus', 'equals', 'extra', 'new',
        'statement'], echo_utterance = 1)
    commands.say(['bits', 'binary', 'or', 'equals', 'flag', 'new',
        'statement'], echo_utterance = 1)
    commands.say(['bits', 'right', 'shift', 'equals', '1\\one', 'new',
        'statement'], echo_utterance = 1)
    commands.say(['bits', 'ampersand', 'equals', 'flag', 'shift', 'left', '1\\one', 'new',
        'statement'], echo_utterance = 1)
    commands.say(['factor', 'star', 'equals', 'N.', 'new',
        'statement'], echo_utterance = 1)
    commands.say(['value', 'slash', 'equals', 'parens', '1\\one',
        'plus', 'discount', 'rate', 'new', 'statement'], echo_utterance = 1)
    commands.say(['rate', 'divide', 'equals', 'time',
        'new', 'statement'], echo_utterance = 1)
    commands.quit(save_speech_files=0, disconnect=0)

add_test('operators', test_operators, 'testing various operators')



##############################################################################
# Testing python support
##############################################################################
def pseudo_python_wrapper():
    test_pseudo_python.test_dictate_from_scratch(testing)

add_test('python', pseudo_python_wrapper, 'testing the various CSCs and LSAs for dictating Python from scratch')

def pseudo_python_editing_wrapper():
    test_pseudo_python.test_editing(testing)

add_test('python_editing', pseudo_python_editing_wrapper, 'testing the various CSCs and LSAs for editing Python')

def python_compilation_wrapper():
    test_pseudo_python.test_python_compilation(testing)

def python_misc_statements_wrapper():
   test_pseudo_python.test_misc_py_statements(testing)

add_test('python_compilation', python_compilation_wrapper, 'testing parsing of python symbols.')



add_test('py_misc_statements', python_misc_statements_wrapper, 'testing miscelleneous python statements.')


##############################################################################
# Testing C/C++ support
##############################################################################
def pseudo_C_Cpp_wrapper():
    test_pseudo_C_Cpp.test_dictate_from_scratch(testing)

add_test('C_Cpp', pseudo_C_Cpp_wrapper, 'testing the various CSCs and LSAs for dictating C/C++ from scratch')

def pseudo_C_Cpp_editing_wrapper():
    test_pseudo_C_Cpp.test_editing(testing)

add_test('C_Cpp_editing', pseudo_C_Cpp_editing_wrapper, 'testing the various CSCs and LSAs for editing C/C++')

def C_Cpp_compilation_wrapper():
    test_pseudo_C_Cpp.test_C_Cpp_compilation(testing)

def C_Cpp_misc_statements_wrapper():
   test_pseudo_C_Cpp.test_misc_C_Cpp_statements(testing)

add_test('C_Cpp_compilation', C_Cpp_compilation_wrapper, 'testing parsing of C/C++ symbols.')

add_test('C_Cpp_misc_statements', C_Cpp_misc_statements_wrapper, 'testing miscelleneous C/C++ statements.')


##############################################################################
# Testing repetition of last commands
##############################################################################


def test_repeat_last():

    global small_buff_c, small_buff_py, large_buff_py

    testing.init_simulator_regression()
    file_name = large_buff_py
    test_command("""open_file(r'""" + file_name + """')""")
    test_command("""say(['after hyphen'])""")
    test_command("""say(['again'])""")
    test_command("""goto_line(1)""")
    test_command("""say(['after hyphen'])""")
    test_command("""say(['again 3 times'])""")
    test_command("""goto_line(1)""")
    test_command("""say(['after hyphen'])""")
    test_command("""say(['3 times'])""")



add_test('repeat_last', test_repeat_last, 'testing repetition of last command')


##############################################################################
# Testing changing direction of previous command
##############################################################################


def test_change_direction():

    global small_buff_c, small_buff_py, large_buff_py

    testing.init_simulator_regression()

    file_name = large_buff_py

    test_command("""open_file(r'""" + file_name + """')""")


    test_command("""say(['after hyphen'])""")
    test_command("""say(['again'])""")
    test_command("""say(['again'])""")
    test_command("""say(['previous one'])""")
    test_command("""say(['previous one'])""")
    test_command("""say(['next', 'one'])""")

add_test('change_direction', test_change_direction, 'testing changing direction of last command')

##############################################################################
# Testing LSA masking by NatSpeak words
##############################################################################


def test_misc_bugs():

    testing.init_simulator_regression()

    test_command("""open_file(r'blah.py')""")

    #
    # NatSpeak defined words like '<\\less-than' used to mask the VoiceCode
    # defined words like '<\\less than' (i.e. no hyphen). Since interpreter
    # didn't know of an LSA with spoken form 'less-than', it treated it as
    # part of a new symbol.
    #
    # This tests a fix which ignores non-alphanums in the spoken form of
    # words to be interpreted.
    #
    test_command("""say(['<\\less-than', '>\\greater-than', '=\\equal-sign'])""")

add_test('misc_bugs', test_misc_bugs, 'Testing a series of miscellaneous bugs that might reoccur.')


##############################################################################
# Testing AppMgr dictionaries
##############################################################################

def manager_state(manager):
    print ''
    print 'state {'
    apps = manager.app_names()
    apps.sort()
    for app in apps:
        print 'application: ', app
        instances =  manager.app_instances(app)
        instances.sort()
        for instance in instances:
            sys.stdout.flush()
            print 'instance: ', instance
            a_name = manager.app_name(instance)
            if a_name != app:
                print 'Warning: app names %s and %s do not match' \
                    % (app, a_name)
            windows = manager.known_windows(instance)
            windows.sort()
#            print 'windows is ', repr(windows), type(windows), type([])
            for window in windows:
                print 'window %d' % (window)
                win_ins = manager.window_instances(window)
#                print repr(win_ins)
                if instance not in win_ins:
                    print 'Warning: instance %s not found in window list' \
                        % instance
                sys.stdout.flush()
    known_windows = manager.known_windows()
    known_windows.sort()
    print 'known windows', known_windows
    print '} state'
    print ''

def instance_status(manager, instance):
    print ''
    if not manager.known_instance(instance):
        print "instance %s is unknown" % instance
        return
    print "instance %s" % instance
    module = manager.instance_module(instance)
    if module != None:
        print "running in module %s" % module
    else:
        print "(unknown module)"
    windows = manager.known_windows(instance)
    windows.sort()
    print "windows: ", windows
    for window in windows:
        print "window #%d:" % window
        if manager.recog_mgr.shared_window(window):
            print "shared"
        if manager.recog_mgr.single_display(window):
            print "single-window display"
        instances = manager.window_instances(window)
        print "all instances for window:"
        for app in instances:
            print app
    print ''

def set_window(current, window, app_name, app = None, alt_title = ""):
    current.set_info(window.ID(), window.module(), app, app_name, alt_title)

def start_recog(manager, current):
    win, title, module = current.window_info()
    manager.recog_mgr._recognition_starting(win, title, module)

def new_buffer_for_instance(instance, buffer, before = "", after = ""):
    print 'new buffer %s for instance %d' % (buffer, int(instance))
    instance.open_file(buffer)
    b = instance.find_buff(buffer)
    b.insert_indent(before, after)
    instance.print_buff_if_necessary()

def new_instance(manager, current, app, window = None,
        alt_title = ""):
    print 'new instance of %s %d' % (app.app_name, app)
    check = 0
    if window != None:
         print 'with window %d' % (window)
         check = 1
         set_window(current, window, app.app_name, app, alt_title)
    i_name = manager.new_instance(app, check)
    a_name = manager.app_name(i_name)
    if a_name != app.app_name:
        print 'Warning: app names %s and %s do not match' \
            % (app.app_name, a_name)
    a = manager.instances[i_name]
    if a != app:
        print 'Warning: AppStates %d and %d do not match' \
            % (app, a)
    return i_name

def new_universal_instance(manager, current, app, exclusive = 1):
    print 'new universal instance of %s %d' % (app.app_name, app)
    i_name = manager.new_universal_instance(app, exclusive)
    a_name = manager.app_name(i_name)
    if a_name != app.app_name:
        print 'Warning: app names %s and %s do not match' \
            % (app.app_name, a_name)
    a = manager.instances[i_name]
    if a != app:
        print 'Warning: AppStates %d and %d do not match' \
            % (app, a)
    return i_name

class FakeWindow(Object.Object):
    def __init__(self, handle, module, **args):
        self.deep_construct(FakeWindow,
                            {'handle': handle,
                             'module_name': module
                            },
                            args)
    def __int__(self):
        return self.handle
    def ID(self):
        return self.handle
    def module(self):
        return self.module_name

class FakeAppState(EdSim.EdSim):
# uses EdSim to handle buffer related stuff, for rsm_algorithm test,
# but overrides shared_window, multiple_window, is_active, title_string,
# so that we can pretend to be a variety of different types of editor
    def __init__(self, value, buff = None, shared = 0,
            multi = 1, active = 1, title_control = 1,
            safe_active = 1, **attrs):
        self.deep_construct(FakeAppState,
                            {'value': value,
                             'the_title_string': '',
                             'shared_windows': shared,
                             'multi': multi,
                             'title_control': title_control,
                             'active': active,
                             'safe_active': safe_active
                            }, attrs)
        if buff != None:
            self.open_file(buff)
    def __str__(self):
        return str(self.value)
    def __int__(self):
        return self.value
    def shared_window(self):
        return self.shared_windows
    def multiple_windows(self):
        return self.multi
#    def name(self):
#        return self.buff
    def suspend(self):
        self.active = 0
    def resume(self):
        self.active = 1
    def is_active(self):
        return self.active
    def is_active_is_safe(self):
        return self.safe_active
    def set_instance_string(self, instance_string):
        self.the_instance_string = instance_string
        return self.title_control
    def instance_string(self):
        if self.title_control:
            return self.the_instance_string
        return None
    def title_escape_sequence(self, a, b):
        return self.title_control

def old_test_am_dictionaries():
    manager = AppMgr.AppMgr()

    Emacs = FakeAppState(1, app_name = 'emacs')
    another_Emacs = FakeAppState(2, app_name = 'emacs')
    yet_another_Emacs = FakeAppState(4, app_name = 'emacs')
    shell_Emacs = FakeAppState(5, app_name = 'emacs')
    shell_Emacs2 = FakeAppState(6, app_name = 'emacs')
    Vim = FakeAppState(3, app_name = 'Vim')
    i = new_instance(manager, Emacs)
    manager_state(manager)
    print 'new window 14'
    manager.new_window(i, 14)
    print 'new window 20'
    manager.new_window(i, 20)
    manager_state(manager)

    j = new_instance(manager, another_Emacs, 10)
    manager_state(manager)

    k = new_instance(manager, 'Vim', Vim)
    print 'delete window 20'
    manager.delete_window(i, 20)
    manager_state(manager)

    print 'delete instance ' + j
    manager.delete_instance(j)
    l = new_instance(manager, 'Emacs', yet_another_Emacs, 7)
    manager_state(manager)
    print 'delete instance ' + i
    manager.delete_instance(i)
    manager_state(manager)
    print 'delete instance ' + k
    manager.delete_instance(k)
    print 'delete instance ' + l
    manager.delete_instance(l)
    manager_state(manager)

    m = new_instance(manager, 'Emacs (Exceed)', shell_Emacs,
        window = 94)
    n = new_instance(manager, 'Emacs (Exceed)', shell_Emacs2,
        window = 94)
    manager_state(manager)
    manager.delete_window(m, 94)
    manager_state(manager)
    manager.delete_instance(m)
    manager_state(manager)
    manager.delete_instance(n)
    manager_state(manager)

def test_am_dictionaries():
    g_factory = sr_grammars.WinGramFactoryDummy(silent = 1)
    GM_factory = GramMgr.WinGramMgrFactory(g_factory)
    current = RecogStartMgr.CurrWindowDummy()
    recog_mgr = RecogStartMgr.RSMExtInfo(GM_factory = GM_factory,
      win_info = current)
    manager = AppMgr.AppMgr(recog_mgr)
    windows = {}
    mod_Emacs = KnownTargetModule.DedicatedModule(module_name = 'EMACS',
        editor = 'emacs')
    mod_Vim = KnownTargetModule.DedicatedModule(module_name = 'VIM',
        editor = 'Vim')
    mod_telnet = KnownTargetModule.RemoteShell(module_name = 'TELNET',
        title_varies = 1)
    manager.add_module(mod_Emacs)
    manager.add_module(mod_Vim)
    manager.add_module(mod_telnet)
    manager.add_prefix('emacs', 'Yak')
    manager.add_prefix('Vim', 'Oldie')

    Emacs = FakeAppState(1, 'a_file.py', app_name = 'emacs')
    another_Emacs = FakeAppState(2, 'poodle.C', app_name = 'emacs')
    yet_another_Emacs = FakeAppState(4, 'foo.bar', app_name = 'emacs')
    shell_Emacs = FakeAppState(5, 'bug.c', app_name = 'emacs', shared = 1, multi = 0)
    shell_Emacs2 = FakeAppState(6, 'dog.q', app_name = 'emacs', shared = 1, multi = 0)
    Vim = FakeAppState(3, 'tests_def.py', app_name = 'Vim')
    i = new_instance(manager, current, Emacs)
    manager_state(manager)
    print 'new window 14'
    windows[14] = FakeWindow(14, 'EMACS')
    set_window(current, windows[14], Emacs.app_name, Emacs)
    manager.new_window(i)
    print 'new window 20'
    manager.app_instance(i).buff = 'dogs.C'
    windows[20] = FakeWindow(20, 'EMACS')
    set_window(current, windows[20], Emacs.app_name, Emacs)
    manager.new_window(i)
    manager_state(manager)

    windows[10] = FakeWindow(10, 'EMACS')
    j = new_instance(manager, current, another_Emacs, windows[10])
    manager_state(manager)

    k = new_instance(manager, current, Vim)
    print 'delete window 20'
    manager.delete_window(i, 20)
    manager_state(manager)

    print 'delete instance ' + j
    manager.delete_instance(j)
    windows[7] = FakeWindow(7, 'EMACS')
    l = new_instance(manager, current, yet_another_Emacs, windows[7])
    manager_state(manager)
    print 'delete instance ' + i
    manager.delete_instance(i)
    manager_state(manager)
    print 'delete instance ' + k
    manager.delete_instance(k)
    print 'delete instance ' + l
    manager.delete_instance(l)
    manager_state(manager)
    windows[94] = FakeWindow(94, 'TELNET')

    m = new_instance(manager, current, shell_Emacs,
        window = windows[94])
    shell_Emacs.suspend()
    n = new_instance(manager, current, shell_Emacs2,
        window = windows[94])
    manager_state(manager)
    manager.delete_window(m, 94)
    manager_state(manager)
    manager.delete_instance(m)
    manager_state(manager)
    manager.delete_instance(n)
    manager_state(manager)

def test_rsm_algorithm(trust = 0):
    g_factory = sr_grammars.WinGramFactoryDummy(silent = 0)
    GM_factory = GramMgr.WinGramMgrFactory(g_factory)
    current = RecogStartMgr.CurrWindowDummy()
    recog_mgr = RecogStartMgr.RSMExtInfo(GM_factory = GM_factory,
        trust_current_window = trust, win_info = current)
    manager = AppMgr.AppMgr(recog_mgr)
    windows = {}
    mod_Emacs = KnownTargetModule.DedicatedModule(module_name = 'EMACS',
        editor = 'emacs')
    mod_Vim = KnownTargetModule.DedicatedModule(module_name = 'VIM',
        editor = 'Vim')
    mod_telnet = KnownTargetModule.RemoteShell(module_name = 'TELNET',
        title_varies = 1)
    mod_exceed = \
        KnownTargetModule.DualModeDisplayByTitle(title_regex = '^Exceed$',
        module_name = 'EXCEED')
    manager.add_module(mod_Emacs)
    manager.add_module(mod_Vim)
    manager.add_module(mod_telnet)
    manager.add_module(mod_exceed)
    manager.add_prefix('emacs', 'Yak')
    manager.add_prefix('Vim', 'Oldie')
    manager.add_prefix('WaxEdit', 'Floor')

    fish_h_before = "void move(float x, y);"
    fish_h_after = "\n"
    fish_before = """/* This is a small test buffer for C */

void move(float x, y)
"""
    fish_after = """{
  move_horiz(x);
  move_vert(y)
  horiz_pos = 0;
  this_sym_is_unres = 0;
  this_sym_is_unres_too = 0;
  this_sym_has_an_other_abbrev = 0;
  f_name;
  f_name2();
  API_function(1);
  API_function(2);
}
"""

    fowl_before = """import sys

def something(value):
    print """
    fowl_after = """value

if __name__ == '__main__':
    something('nice')
"""

    dog_before = """#!/usr/local/bin/perl5


#
# Environment variables for voiceGrip
#
$voiceGripHome = $ENV{'VGTWO'};
$voiceGripOS = $ENV{'VGOS'};
if ($voiceGripOS eq 'win') {
    $dirSep = "\\";
    $curDirCom = 'cd';
} else {
    $dirSep = """
    dog_after = """'/';
    $curDirCom = 'pwd';
};
"""


    Emacs = FakeAppState(1, 'a_file.py', app_name = 'emacs')
    another_Emacs = FakeAppState(2, 'poodle.C', app_name = 'emacs')
#    yet_another_Emacs = FakeAppState(4, 'foo.bar', app_name = 'emacs')
    shell_Emacs = FakeAppState(5, 'bug.c', app_name = 'emacs', shared = 1, multi = 0,
        title_control = 0)
    shell_Emacs2 = FakeAppState(6, 'dog.q', app_name = 'emacs', shared = 1, multi = 0,
        title_control = 0)
#    Vim = FakeAppState(3, 'tests_def.py')
    text_Emacs = FakeAppState(7, 'nothing.py', app_name = 'emacs', shared = 1, multi = 0,
        title_control = 0)
    text_Emacs2 = FakeAppState(8, 'pickle.dll', app_name = 'emacs', shared = 1, multi = 0,
        title_control = 0)
    xEmacs = FakeAppState(9, '.cshrc', app_name = 'emacs', shared = 0, multi = 1)
    text_Vim = FakeAppState(10, '', app_name = 'Vim', shared = 1, multi = 0, safe_active = 0)
    internal = FakeAppState(12, 'large_buff.py', app_name = 'WaxEdit', multi = 0)

    windows[14] = FakeWindow(14, 'EMACS')
    print 'new instance in window 14'
    e1 = new_instance(manager, current, Emacs, windows[14])
    instance_status(manager, e1)
    windows[20] = FakeWindow(20, 'EMACS')
    windows[50] = FakeWindow(50, 'BROWSEUI')
    print 'new window 20'
    set_window(current, windows[20], Emacs.app_name, Emacs)
    new_buffer_for_instance(Emacs, "fish.C", before = fish_before,
        after = fish_after)
    instance_status(manager, e1)
    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, e1)

    print '\nSetting text mode on'
    manager.recog_mgr.set_text_mode(1)
    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)

    print '\nSetting text mode off'
    manager.recog_mgr.set_text_mode(0)
    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)


    set_window(current, windows[50], None, alt_title = 'D:\Projects')
    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, e1)

    windows[5] = FakeWindow(5, 'TELNET')
    windows[8] = FakeWindow(8, 'TELNET')
    print 'new instance in telnet window 5'
    se1 = new_instance(manager, current, shell_Emacs,
        windows[5])
    instance_status(manager, se1)
    manager_state(manager)
    print 'now specifying window'
    if manager.specify_window(se1):
        print 'success'
    else:
        print 'failed'
    instance_status(manager, se1)

    set_window(current, windows[8], None, alt_title = 'ttssh - acappella')
    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, se1)

    set_window(current, windows[5], shell_Emacs.app_name, shell_Emacs,
        alt_title = 'ttssh - acappella')
    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, se1)

    print 'suspending ', se1
    shell_Emacs.suspend()
    set_window(current, windows[5], None, alt_title = 'ttssh - acappella')
    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, se1)

    se2 = new_instance(manager, current, shell_Emacs2)
    instance_status(manager, se2)
    instance_status(manager, se1)
    print 'now specifying window'
    if manager.specify_window(se2):
        print 'success'
    else:
        print 'failed'
    instance_status(manager, se2)
    instance_status(manager, se1)
    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, se1)
    instance_status(manager, se2)
    print 'suspending ', se2
    shell_Emacs2.suspend()

    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, se1)
    instance_status(manager, se2)

    print 'resuming ', se1
    shell_Emacs.resume()
    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, se1)
    instance_status(manager, se2)

    windows[15] = FakeWindow(15, 'EXCEED')
    print 'new Vim instance in exceed window 15'
    tv1 = new_instance(manager, current, text_Vim,
        windows[15], alt_title = 'xterm - acappella')
    new_buffer_for_instance(text_Vim, "dog.pl", before = dog_before,
        after = dog_after)
    instance_status(manager, tv1)
    manager_state(manager)
    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, tv1)

    print 'suspending ', tv1
    text_Vim.suspend()
    set_window(current, windows[15], None, alt_title = 'xterm - acappella')

    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, tv1)

    print 'new emacs instance in exceed window 15'
    te1 = new_instance(manager, current, text_Emacs,
        windows[15], alt_title = 'xterm - acappella')
    instance_status(manager, te1)

    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, te1)

    print 'now specifying window'
    if manager.specify_window(te1):
        print 'success'
    else:
        print 'failed'
    instance_status(manager, te1)

    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, te1)

    print 'suspending ', te1
    text_Emacs.suspend()
    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, te1)

    print 'resuming ', tv1
    text_Vim.resume()
    set_window(current, windows[15], text_Vim.app_name, text_Vim,
        alt_title = 'xterm - acappella')
    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, tv1)

    windows[25] = FakeWindow(25, 'EXCEED')
    print 'new emacs instance in exceed window 25'
    xe1 = new_instance(manager, current, xEmacs,
        windows[25])
    instance_status(manager, xe1)
    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, xe1)

    print 'app reports new window (is current)'
    windows[26] = FakeWindow(26, 'EXCEED')
    set_window(current, windows[26], xEmacs.app_name, xEmacs)
    print 'current is', repr(current.window_info())
    manager.new_window(xe1)
    instance_status(manager, xe1)

    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, xe1)

    text_Vim.suspend()
    set_window(current, windows[15], None, alt_title = 'xterm - acappella')
    print 'app reports new window (is not current)'
    print 'current is', repr(current.window_info())
    manager.new_window(xe1)
    instance_status(manager, xe1)
    instance_status(manager, tv1)

    windows[27] = FakeWindow(27, 'EXCEED')

    print 'but now it is'
    set_window(current, windows[27], xEmacs.app_name, xEmacs)
    print 'current is', repr(current.window_info())
    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, xe1)

    windows[99] = FakeWindow(99, 'PYTHON')
    ii1 = new_universal_instance(manager, current, internal)
    print 'now it is on WaxEdit'
    set_window(current, windows[99], internal.app_name, internal)
    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, ii1)

    print 'but now it is'
    set_window(current, windows[27], xEmacs.app_name, xEmacs)
    print 'current is', repr(current.window_info())
    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, xe1)
    instance_status(manager, ii1)
    manager.delete_instance(ii1)

    print 'and now the WaxEdit is gone'
    print 'current is', repr(current.window_info())
    print 'starting recognition in ', repr(current.window_info())
    start_recog(manager, current)
    instance_status(manager, xe1)

def test_rsm_algorithm_no_trust():
    test_rsm_algorithm(trust = 0)

def test_rsm_algorithm_trust():
    test_rsm_algorithm(trust = 1)

add_test('am_dictionaries', test_am_dictionaries,
    'Testing AppMgr dictionary management.')

add_test('rsm_algorithm', test_rsm_algorithm_no_trust,
    'Testing RecogStartMgr algorithm.')

add_test('rsm_algorithm_trust', test_rsm_algorithm_trust,
    'Testing RecogStartMgr algorithm.')

##############################################################################
# Testing WinGramMgr with dummy grammars
##############################################################################

def activate_for(manager, buffer, window):
    print 'activating buffer %s for window %d' % (buffer, window)
    manager.app.change_buffer(buffer)
    manager.app.print_buff_if_necessary()
    manager.activate(buffer, window)

def new_buffer(manager, buffer, window = None, before = "", after = ""):
    print 'new buffer %s' % (buffer)
    if window != None:
         print 'with window %d' % (window)
    manager.app.open_file(buffer)
    b = manager.app.find_buff(buffer)
    b.insert_indent(before, after)
    manager.new_buffer(buffer, window)

def new_window(manager, window):
    print 'new window %d' % (window)
    manager.new_window(window)

def delete_window(manager, window):
    print 'delete window %d' % (window)
    manager.delete_window(window)

def buffer_closed(manager, buffer):
    print 'close buffer %s' % (buffer)
    manager.buffer_closed(buffer)

def test_gram_manager_flags(global_grammars = 0, exclusive = 0):
    fish_h_before = "void move(float x, y);"
    fish_h_after = "\n"
    fish_before = """/* This is a small test buffer for C */

void move(float x, y)
"""
    fish_after = """{
  move_horiz(x);
  move_vert(y)
  horiz_pos = 0;
  this_sym_is_unres = 0;
  this_sym_is_unres_too = 0;
  this_sym_has_an_other_abbrev = 0;
  f_name;
  f_name2();
  API_function(1);
  API_function(2);
}
"""

    fowl_before = """import sys

def something(value):
    print """
    fowl_after = """value

if __name__ == '__main__':
    something('nice')
"""

    dog_before = """#!/usr/local/bin/perl5


#
# Environment variables for voiceGrip
#
$voiceGripHome = $ENV{'VGTWO'};
$voiceGripOS = $ENV{'VGOS'};
if ($voiceGripOS eq 'win') {
    $dirSep = "\\";
    $curDirCom = 'cd';
} else {
    $dirSep = """
    dog_after = """'/';
    $curDirCom = 'pwd';
};
"""

    factory = sr_grammars.WinGramFactoryDummy()
    app = EdSim.EdSim(multiple = 1, instance_reporting = 1)
    manager = GramMgr.WinGramMgr(factory, app = app,
        instance_name = None, recog_mgr = None,
        global_grammars = global_grammars, exclusive = exclusive)
    w = 5
    w2 = 7
    new_buffer(manager, 'fish.C', w, fish_before, fish_after)
    new_buffer(manager, 'fowl.py', w, fowl_before, fowl_after)
    activate_for(manager, 'fish.C', w)
    new_window(manager, w2)
    new_buffer(manager, 'dog.pl', w2, dog_before, dog_after)
    new_buffer(manager, 'fish.h', w2, fish_h_before, fish_h_after)
    activate_for(manager, 'dog.pl', w2)
    activate_for(manager, 'fish.h', w2)
    activate_for(manager, 'fowl.py', w)
    app.close_buffer('fowl.py', -1)
    buffer_closed(manager, 'fowl.py')
    print 'deactivate all for window %d' % (w)
    manager.deactivate_all(w)
    delete_window(manager, w2)
    app.close_buffer('dog.pl', -1)
    buffer_closed(manager,  'dog.pl')
    activate_for(manager,'fish.C', w)
    print 'deactivate all'
    manager.deactivate_all()
    print 'close all buffers'
    app.close_all_buffers(-1)
    print 'cleanup app'
    app.cleanup()
    print 'cleanup manager'
    manager.cleanup()
    print 'test ending - expect dels of manager, app'

def test_gram_manager():
    test_gram_manager_flags()

def test_gram_manager_all_set():
    test_gram_manager_flags(1, 1)


add_test('dummy_grammars', test_gram_manager,
   'Testing WinGramMgr grammar management with dummy grammars.')
add_test('dummy_grammars_global', test_gram_manager_all_set,
   'Testing WinGramMgr grammar management with global, exclusive dummy grammars.')

##############################################################################
# Testing EdSim allocation and cleanup
##############################################################################

def test_EdSim_alloc_cleanup():
    #
    # Create a command interpreter connected to the editor simulator
    #

    print '\n*** testing cleanup with single buffer EdSim\n'
#    natlink.natConnect()
    editor = EdSim.EdSim(instance_reporting = 1)
    temp_config = temp_factory.new_config(editor = editor, skip_config = 1)
    del editor
#     a_mediator = MediatorObject.MediatorObject(app =
#        EdSim.EdSim(instance_reporting = 1),
#         interp=CmdInterp.CmdInterp())
    test_CmdInterp_mediator(temp_config)

    print '\n*** testing cleanup with multi-buffer EdSim\n'
#    natlink.natConnect()
    editor = EdSim.EdSim(multiple = 1, instance_reporting = 1)
    temp_config = temp_factory.new_config(editor = editor, skip_config = 1)
    del editor
#     a_mediator = MediatorObject.MediatorObject(app =
#        EdSim.EdSim(multiple = 1, instance_reporting = 1),
#         interp=CmdInterp.CmdInterp())
    test_CmdInterp_mediator(temp_config)


add_test('EdSim_alloc_cleanup', test_EdSim_alloc_cleanup,
    'Testing EdSim allocation and cleanup.')

##############################################################################
# Testing manual symbol formatting and styling
##############################################################################

def test_symbol_formatting():
    testing.init_simulator_regression()

    commands.open_file('blah.py')

    print "Testing styles\n"

    test_say(['ordinary', 'formatting', 'equals', 'one'])
    commands.say(['new', 'statement'])

    test_say(['lower', 'Hungarian', 'ugly', 'Java', 'convention'])
    commands.say(['new', 'statement'])

    test_say(['animal', 'equals', 'Hungarian', 'friendly', 'dolphin',
        'empty', 'function'])
    commands.say(['new', 'statement'])

    test_say(['\\All-Caps', 'style', 'W.', 'M.', 'user'])
    commands.say(['new', 'statement'])

    print "Testing styling across utterances"
    test_say(['Hungarian'])

    test_say(['delayed', 'name'])
    commands.say(['new', 'statement'])

    print "Testing manual formatting\n"

    test_say(['no', 'space', 'style', 'W.', 'X.', 'cap', 'python'])
    commands.say(['new', 'statement'])

    test_say(['send', 'to', '\\Caps-On', 'the', 'boss'])
    commands.say(['new', 'statement'])

    test_say(['caps', 'on', 'matters', '\\No-Caps', 'of', 'state'])
    commands.say(['new', 'statement'])

    test_say(['inter', 'caps', 'fish', '_\\underscore', 'food'])
    commands.say(['new', 'statement'])

    test_say(['Hungarian', 'notation', '\\No-Caps', 'dog', 'food'])
    commands.say(['new', 'statement'])

    print 'Making sure manual underscores suppress automatic ones'
    test_say(['_\\underscore', 'private', '_\\underscore', 'variable',
        '_\\underscore'])
    commands.say(['new', 'statement'])

    print 'Making sure consecutive letters are treated as part of the same word'
    test_say(['windows', 'X.', 'P.'])
    commands.say(['new', 'statement'])


add_test('symbol_formatting', test_symbol_formatting,
    'Testing styling and manual formatting of new symbols.')

##############################################################################
# Testing commands that specify formatting of subsequent symbols
##############################################################################

def test_commands_that_set_format_of_subsequent_symbols():
    testing.init_simulator_regression()

    print 'Testing Python commands.'

    commands.open_file('blah.py')
    test_say(['class', 'some', 'python', 'class', 'inherits', 'from', 'some', 'python', 'super', 'class'])

    print 'Testing C++ commands.'

    commands.open_file('blah.cpp')
    test_say(['class', 'some', 'C' 'class', 'inherits', 'from', 'some', 'C', 'super', 'class'])



add_test('commands_that_set_format_of_subsequent_symbols', test_commands_that_set_format_of_subsequent_symbols,
    'Testing commands that specify formatting of subsequent symbols.')



##############################################################################
# Testing basic correction features of ResMgr
##############################################################################
def check_stored_utterances(instance_name, expected):
    sys.stdout.flush()
    the_mediator = testing.mediator()
    n = the_mediator.stored_utterances(instance_name)
    if n == expected:
        print '\n%d stored utterances, as expected\n' %n
    else:
        print '\nWARNING: %d stored utterances (expected %d)' % (n, expected)

    recent = the_mediator.recent_dictation(instance_name)
    if expected == 0:
        if recent == None:
            print '\nrecent dictation is empty, as expected\n'
        else:
            msg = \
                '\nWARNING: %d recently dictated utterances (expected None)' \
                % len(recent)
            print msg
    else:
        n = 0
#        print 'recent is %s' % repr(recent)
        if recent != None:
            n = len(recent)
        if n != expected:
            msg = \
                '\nWARNING: %d recently dictated utterances (expected %d)' \
                % (n, expected)
            print msg
        else:
            print '\n%d recently dictated utterances, as expected\n' \
                % n
    sys.stdout.flush()

def check_recent(instance_name, expected_utterances, expected_status):
    debug.trace('tests_def.check_recent',
                 'expected_utterances=%s, expected_status=%s' %
                 (repr(expected_utterances), repr(expected_status)))
    sys.stdout.flush()
    the_mediator = testing.mediator()
    n = the_mediator.stored_utterances(instance_name)
    recent = the_mediator.recent_dictation(instance_name)

    expected = len(expected_utterances)
    n_compare = expected
    if n != expected:
        msg = '\nWARNING: check_recent found %d stored utterances\n' % n
        msg = msg +  '(expected %d)' % expected
        print msg
        n_compare = min(n, expected)
    for i in range(1, n_compare+1):
        expect = expected_utterances[-i]
#        expect = string.split(expected_utterances[-i])
        received = recent[-i][0].spoken_forms()
        status = recent[-i][2]
        if expect != received:
            print "\nWARNING: utterance %d doesn't match:\n" % i
            print "expected:\n"
            print expect
            print "received:\n"
            print received
        if expected_status[-i] != status:
            msg = "\nWARNING: status of utterance "
            msg = msg + "%d (%s) was %d (expected %d)" \
                % (i, expect, status, expected_status[-i])
            print msg
    if n < expected:
        print '\nadditional utterances were expected:'
        for i in range(n_compare + 1, expected):
            print string.split(expected_utterances[-i])
    elif n > expected:
        print '\nextra utterances were received:'
        for i in range(n_compare + 1, n):
            received = recent[-i][0].spoken_forms()
            print received
    sys.stdout.flush()

def check_scratch_recent(instance_name, n = 1, should_fail = 0):
    print 'scratching %d\n' %n
    sys.stdout.flush()

    the_mediator = testing.mediator()
    scratched = the_mediator.scratch_recent(instance_name, n = n)
    msg = 'scratch %d ' % n
    if scratched == n:
        msg = msg + 'succeeded '
        if should_fail:
            msg = 'WARNING: ' +  msg + 'unexpectedly'
        else:
            msg = msg + 'as expected'
    else:
        msg = msg + 'failed '
        if should_fail:
            msg = msg + 'as expected'
        else:
            msg = 'WARNING: ' +  msg + 'unexpectedly'
    print msg

    sys.stdout.flush()
    return scratched

def check_recent_symbols(instance_name, mess = ""):
    the_mediator = testing.mediator()

    mess = "%s\nRecent interpreted symbols were: " % mess
    print mess
    for a_sym in the_mediator.recent_symbols(instance_name):
       debug.trace('tests_def.check_recent_symbols', '** a_sym=%s' % repr(a_sym))
       print "   written as: '%s', spoken as: '%s'" % \
             (a_sym.native_symbol(), repr(a_sym.spoken_phrase()))



def test_reinterpret(instance_name, changed, user_input = None):
    the_mediator = testing.mediator()
    done = None
    try:
        if user_input:
            #
            # Create temporary user input file
            #
            old_stdin = sys.stdin
            temp_file_name = vc_globals.tmp + os.sep + 'user_input.dat'
            temp_file = open(temp_file_name, 'w')
#        print 'temp file opened for writing'
            sys.stdout.flush()
            temp_file.write(user_input)
            temp_file.close()
            temp_file = open(temp_file_name, 'r')
#        print 'temp file opened for reading'
            sys.stdout.flush()
            sys.stdin = temp_file

        done = the_mediator.reinterpret_recent(instance_name, changed)

    finally:
        if user_input:
            sys.stdin = old_stdin
            temp_file.close()
    return done



def correct_recent_symbols(instance_name, corrections, user_input):
    sys.stdout.flush()
    the_mediator = testing.mediator()
    the_mediator.correct_recent_symbols(instance_name, corrections)


def reinterpret(instance_name, utterances, errors, user_input = None,
    should_fail = 0):
    sys.stdout.flush()
    the_mediator = testing.mediator()
    n = the_mediator.stored_utterances(instance_name)
    recent = the_mediator.recent_dictation(instance_name)
    n_recent = 0
    if recent != None:
        n_recent = len(recent)

    earliest = max(errors.keys())
    if n < earliest:
        print "\ncan't correct error %d utterances ago" % earliest
        print "because stored_utterances only goes back %d\n" % n
        sys.stdout.flush()
        return 0
    if n_recent < earliest:
        print "\ncan't correct error %d utterances ago" % earliest
        print "because recent_dictation only goes back %d\n" % n_recent
        sys.stdout.flush()
        return 0
    okay = 1
    for i in errors.keys():
        if not the_mediator.can_reinterpret(instance_name, i):
            print "\ndon't expect to be able to correct error %d utterances ago" % i
            print "because can_reinterpret returned false, but we'll try anyway\n"
            sys.stdout.flush()
    if not okay:
        return 0

    print 'detecting changes'
    sys.stdout.flush()
    changed = []
    changed_numbers = []
    for i, change in errors.items():
        print 'utterance %d: change = %s' % (i, repr(change))
        sys.stdout.flush()
        utterance = recent[-i][0]
        number = recent[-i][1]
        spoken = utterance.spoken_forms()
        wrong = 0
        for j in range(len(spoken)):
            try:
                replacement = change[spoken[j]]
                print 'word %s being replaced with %s' % (spoken[j], replacement)
                sys.stdout.flush()
                spoken[j] = replacement
                wrong = 1
            except KeyError:
                pass
        if wrong:
            changed.append(i)
            changed_numbers.append(number)
# set_spoken doesn't cause adaption
            print 'utterance %d was changed ' % i
            sys.stdout.flush()
            utterance.set_spoken(spoken)
#            utterances[-i] = string.join(utterance.spoken_forms())
            utterances[-i] = utterance.spoken_forms()
            print 'utterance %d was corrected' % i
            sys.stdout.flush()
    done = None
    if changed_numbers:
        print 'about to reinterpret'
        sys.stdout.flush()
        done = test_reinterpret(instance_name, changed_numbers, user_input = user_input)

    if done == None:
        if should_fail:
            print '\nreinterpretation failed, as expected\n'
        else:
            print '\nWARNING: reinterpretation failed unexpectedly\n'
        sys.stdout.flush()
        return 0
    if testing.correction_available() == 'basic':
        if done == range(max(changed), 0, -1):
            print '\nall utterances from %d to the present'% max(changed)
            print 'were reinterpreted, as expected\n'
            sys.stdout.flush()
            return 1
        else:
            print '\WARNING: only utterances ', done, 'were reinterpreted'
            print '(expected %d to the present)\n' % max(changed)
            sys.stdout.flush()
            return 0
    else:
        okay = 1
        for i in changed:
            if i not in done:
                print '\nWARNING: utterance %d was not reinterpreted\n'
                okay = 0
        for i in done:
            if i not in changed:
                print '\nWARNING: utterance %d was unexpectedly reinterpreted\n'
                okay = 0
        sys.stdout.flush()
        return okay

def check_symbol(interpreter, symbol, expected = 1):
    known = interpreter.known_symbol(symbol)
    if known:
        spoken = interpreter.spoken_forms(symbol)
        print "symbol %s has spoken forms %s" % (symbol, spoken)
        if not expected:
            print "WARNING: symbol %s should not exist" % symbol
    else:
        if expected:
            print "WARNING: symbol %s was expected, but does not exist" \
                % symbol
        else:
            print "symbol %s did not exist, as expected" % symbol
    return known == expected

def test_basic_correction():
    testing.init_simulator_regression()
    instance_name = testing.instance_name()
    correction_available = testing.correction_available()
    if not correction_available:
        msg = '\n***No correction available: '
        msg = msg + 'unable to test correction features***\n'
        print msg
        return
    commands.open_file('blah.py')

    the_mediator = testing.mediator()
    print '\n***Testing initial state***\n'

    check_stored_utterances(instance_name, expected = 0)

    print '\n***Some simple dictation***\n'

    utterances = []
    utterances.append(string.split('class clown inherits from student'))
    input = ['0\n0\n']
    status = [1]

    utterances.append(string.split('class body'))
    input.append('')
    status.append(1)

    utterances.append(string.split('define method popularity method body'))
    input.append('0\n')
    status.append(1)


    utterances.append(string.split('return eight'))
    input.append('')
    status.append(1)

    for i in range(len(utterances)):
        test_say(utterances[i], user_input = input[i], never_bypass_sr_recog=1)

    print '\n***Testing state***\n'

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    print '\n***Testing scratch that***\n'

    scratched = check_scratch_recent(instance_name)
    if scratched:
        del utterances[-scratched:]
        del input[-scratched:]
        del status[-scratched:]

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    print "\n***Moving cursor manually***\n"
    commands.goto_line(1)

    print '\n***Testing scratch that following manual move***\n'

    scratched = check_scratch_recent(instance_name)
    if scratched:
        del utterances[-scratched:]
        del input[-scratched:]
        del status[-scratched:]

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    utterances.append(string.split('define method grades method body return B.'))
    input.append('0\n2\n')
    status.append(1)

    test_say(utterances[-1], user_input = input[-1], never_bypass_sr_recog=1)

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    # AD: With NatSpeak 8, the word Clown must be capitalised in the utterance
    #     otherwise the selection grammar won't intercept it. Silly recognitionMimic()!
    #
    test_say(['select', 'Clown'], never_bypass_sr_recog=1)
    editor = the_mediator.editors.app_instance(instance_name)
    buffer = editor.curr_buffer()

    print '\n***Manually changing text\n'

    buffer.insert('president')
    commands.show_buff()


    status = len(status)*[0]

    print '\n***Testing scratch that following manual change***\n'

    scratched = check_scratch_recent(instance_name, should_fail = 1)
    if scratched:
        del utterances[-scratched:]
        del input[-scratched:]
        del status[-scratched:]

    the_mediator.reset_results_mgr()
    editor = the_mediator.editors.app_instance(instance_name)
    editor.init_for_test(save=-1)

    commands.open_file('blahblah.py')

    interpreter = the_mediator.interpreter()
    print '\n***Testing whether symbol "Cloud" exists before dictation***\n'

    if not check_symbol(interpreter, 'Cloud', expected = 0):
        print 'WARNING: since symbol "Cloud" already exists, '
        print 'the check after correction may report that it exists'
        print 'unexpectedly'


    utterances = []
    utterances.append(string.split('class cloud inherits from student'))
    input = ['0\n0\n']
    status = [1]

    utterances.append(string.split('class body'))
    input.append('')
    status.append(1)

    utterances.append(string.split('fine method popularity method body'))
    input.append('0\n')
    status.append(1)

    utterances.append(string.split('return eight'))
    input.append('')
    status.append(1)

    for i in range(len(utterances)):
        test_say(utterances[i], user_input = input[i], never_bypass_sr_recog=1)

    print '\n***Testing state***\n'

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    print '\n***Testing symbol addition***\n'


    check_symbol(interpreter, 'Cloud')
    check_symbol(interpreter, 'fine_method_popularity')

    print '\n***Testing correction of recent utterance***\n'

    errors = {}
    errors[len(utterances)-2] = {'fine': 'define'}
    reinterpret(instance_name, utterances, errors, user_input = '0\n')

    print '\n***Testing state***\n'

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    print '\n***Testing symbol removal on correction***\n'

    check_symbol(interpreter, 'fine_method_popularity', expected = 0)

    print '\n***Testing correction of another recent utterance***\n'


    errors = {}
    errors[len(utterances)] = {'cloud': 'clown'}
    reinterpret(instance_name, utterances, errors, user_input = '0\n'*3)

    print '\n***Testing state***\n'

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    print '\n***Testing symbol removal on correction***\n'

    check_symbol(interpreter, 'Cloud', expected = 0)

    print '\n***Testing whether symbol "excess" exists before dictation***\n'

    if not check_symbol(interpreter, 'excess', expected = 0):
        print 'WARNING: since symbol "excess" already exists, '
        print 'the check after correction may report that it exists'
        print 'unexpectedly'


    new_utterances = []
    new_utterances.append(string.split('new line'))
    new_input = ['']
    new_status = [1]

    new_utterances.append(['back indent'])
    new_input.append('')
    new_status.append(1)

    new_utterances.append(string.split('excess equals zero'))
    new_input.append('0\n')
    new_status.append(1)

    for i in range(len(new_utterances)):
        test_say(new_utterances[i], user_input = new_input[i], never_bypass_sr_recog=1)

    print '\n***Testing symbol addition***\n'

    check_symbol(interpreter, 'excess', expected = 1)

    editor = the_mediator.editors.app_instance(instance_name)
    buffer = editor.curr_buffer()

    utterances.extend(new_utterances)
    input.extend(new_input)
    status.extend(new_status)

    print '\n***Manually changing text\n'

    pos = buffer.cur_pos()
    buffer.delete(range = (pos-4, pos))
    commands.show_buff()
    status = len(status)*[0]

    print '\n***Testing state***\n'

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    print '\n***Testing failed correction of a recent utterance***\n'

    errors = {}
    errors[1] = {'excess' : 'success'}
    reinterpret(instance_name, utterances, errors, should_fail = 1,
        user_input = '0\n'*10)

    print '\n***Testing failed symbol addition***\n'
    print '(reinterpretation should have failed, so symbol "success" should'
    print 'not have been added)\n'

    check_symbol(interpreter, 'success', expected = 0)

    print '\n***Testing symbol removal***\n'
    print '(despite re-interpretation failure, incorrect symbol "excess"'
    print 'should have been removed)\n'

    check_symbol(interpreter, 'excess', expected = 0)

    print '\n***Testing whether symbol "results" exists before dictation***\n'

    if not check_symbol(interpreter, 'results', expected = 0):
        print 'WARNING: since symbol "results" already exists, '
        print 'the check after correction may report that it exists'
        print 'unexpectedly'

    print '\n***Fixing error manually***\n'

    pos = buffer.cur_pos()
    buffer.delete(range = (pos-6, pos))
    commands.show_buff()
    status = len(status)*[0]

    print '\n***Testing state***\n'

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    new_utterances = []
    new_utterances.append(string.split('excess equals one new line'))
    new_input = ['0\n']
    new_status = [1]

    new_utterances.append(['back indent'])
    new_input.append('')
    new_status.append(1)

    new_utterances.append(string.split('results at index zero jump out equals zero'))
    new_input.append('0\n')
    new_status.append(1)


    for i in range(len(new_utterances)):
#        if new_utterances[i] == 'back indent':
#            test_say([new_utterances[i]], user_input = new_input[i], never_bypass_sr_recog=1)
#        else:
#            split = string.split(new_utterances[i])
#            test_say(split, user_input = new_input[i], never_bypass_sr_recog=1)
        test_say(new_utterances[i], user_input = new_input[i], never_bypass_sr_recog=1)

    utterances.extend(new_utterances)
    input.extend(new_input)
    status.extend(new_status)

    print '\n***Testing state***\n'

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    print '\n***Testing symbol addition***\n'

    check_symbol(interpreter, 'excess', expected = 1)
    check_symbol(interpreter, 'results', expected = 1)

    print '\n***Testing scratch that***\n'

    scratched = check_scratch_recent(instance_name)
    if scratched:
        del utterances[-scratched:]
        del input[-scratched:]
        del status[-scratched:]

    print '\n***Testing state***\n'

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    print '\n***Testing symbol removal***\n'

    check_symbol(interpreter, 'results', expected = 0)

    print '\n***Testing correction after scratch that***\n'

    errors = {}
    errors[2] = {'excess': 'success'}
    reinterpret(instance_name, utterances, errors, user_input = '0\n')

    print '\n***Testing state***\n'

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    print '\n***Testing symbol addition***\n'

    check_symbol(interpreter, 'success', expected = 1)

    print '\n***Testing symbol removal***\n'

    check_symbol(interpreter, 'excess', expected = 0)

    print '\nTesting correction and formatting state:\n'

    new_utterances = []
    new_utterances.append(string.split('new statement'))
    new_input = ['0\n']
    new_status = [1]

    new_utterances.append(string.split('new class'))
    new_input.append('')
    new_status.append(1)

    new_utterances.append(string.split('red brick'))
    new_input.append('0\n')
    new_status.append(1)


    for i in range(len(new_utterances)):
        test_say(new_utterances[i], user_input = new_input[i],
            never_bypass_sr_recog=1)

    utterances.extend(new_utterances)
    input.extend(new_input)
    status.extend(new_status)

    print '\n***Testing state***\n'

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    print '\n***Testing scratch that***\n'

    scratched = check_scratch_recent(instance_name)
    if scratched:
        del utterances[-scratched:]
        del input[-scratched:]
        del status[-scratched:]

    print '\n***new class name should still be in HungarianNotation***\n'

    new_utterances = []
    new_utterances.append(string.split('red book'))
    new_input = ['0\n']
    new_status = [1]

    for i in range(len(new_utterances)):
        test_say(new_utterances[i], user_input = new_input[i],
            never_bypass_sr_recog=1)

    utterances.extend(new_utterances)
    input.extend(new_input)
    status.extend(new_status)

    print '\n***Testing state***\n'

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    new_utterances = []
    new_utterances.append(string.split('class body'))
    new_input = ['0\n']
    new_status = [1]

    new_utterances.append(string.split('define method'))
    new_input.append('')
    new_status.append(1)

    new_utterances.append(string.split('lower Hungarian'))
    new_input.append('')
    new_status.append(1)

    new_utterances.append(string.split('act up'))
    new_input.append('')
    new_status.append(1)

    for i in range(len(new_utterances)):
        test_say(new_utterances[i], user_input = new_input[i],
            never_bypass_sr_recog=1)

    utterances.extend(new_utterances)
    input.extend(new_input)
    status.extend(new_status)

    print '\n***Testing state***\n'

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    print '\n***Testing symbol addition and formatting**\n'
    check_symbol(interpreter, 'actUp', expected = 1)

    print '\n***Testing correction***\n'

    errors = {}
    errors[1] = {'up': 'out'}
    reinterpret(instance_name, utterances, errors, user_input = '0\n'*3)

    print '\n***Testing state***\n'

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    print '\n***Testing symbol removal on correction***\n'

    check_symbol(interpreter, 'actUp', expected = 0)

    print '\n***Testing symbol addition and formatting**\n'
    check_symbol(interpreter, 'actOut', expected = 1)

    print "\n***Moving cursor manually***\n"
    commands.goto_line(1)

    print '\n***Testing scratching of manual styling***\n'

    scratched = check_scratch_recent(instance_name, n = 2)
    if scratched:
        print 'scratched: '
        for i in range(1, scratched+1):
            print utterances[-i]
        del utterances[-scratched:]
        del input[-scratched:]
        del status[-scratched:]

    print '\n***Testing symbol removal on correction***\n'

    check_symbol(interpreter, 'actOut', expected = 0)

    new_utterances = []
    new_input = []
    new_status = []

    new_utterances.append(string.split('act out'))
    new_input.append('')
    new_status.append(1)

    for i in range(len(new_utterances)):
        test_say(new_utterances[i], user_input = new_input[i],
            never_bypass_sr_recog=1)

    utterances.extend(new_utterances)
    input.extend(new_input)
    status.extend(new_status)

    print '\n***Testing state***\n'

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    print '\n***Testing symbol addition and formatting**\n'
    check_symbol(interpreter, 'act_out', expected = 1)

    print '\n***Testing reformatting of a new symbol to another new symbol***\n'

    utterances.append(string.split('new statement return size of promised tax cuts'))
    input.append('0\n')
    status.append(1)
    test_say(utterances[-1], input[-1], never_bypass_sr_recog=1)

    print "Checking that 'excess' is not in recent symbols anymore:"
    check_recent_symbols(instance_name)


    sym_corrections = {}
    sym_corrections[1] = ('size of promised tax cuts', 'size_of_promised_tax_cuts', 'sz_of_prom_tax_cuts')
    correct_recent_symbols(instance_name, sym_corrections, user_input = '0\n')

    print '\n***Testing state***\n'

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    print '\n***Testing tentative symbol removal and correct symbol addition on correction***\n'

    check_symbol(interpreter, 'size_of_promised_tax_cuts', expected = 1)
    check_symbol(interpreter, 'sz_of_prom_tax_cuts', expected = 1)

    print '\n***Testing subsequent dictation of reformatted symbol***\n'

    utterances.append(string.split('new statement size of promised tax cuts equals zero'))
    input.append('0\n')
    status.append(1)
    test_say(utterances[-1], input[-1], never_bypass_sr_recog=1)

    print '\n***Testing reformatting of an existing symbol to a new symbol***\n'

    check_symbol(interpreter, 'RedBook', expected = 1)
    check_symbol(interpreter, 'red_bk', expected = 0)

    utterances.append(string.split('new statement red book equals none'))
    input.append('0\n')
    status.append(1)
    test_say(utterances[-1], input[-1], never_bypass_sr_recog=1)

    sym_corrections = {}
    sym_corrections[1] = ('red book', 'RedBook', 'red_bk')
    correct_recent_symbols(instance_name, sym_corrections, user_input = '0\n')

    print '\n***Testing state***\n'

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    print '\n***Testing correct new symbol addition on correction***\n'

    check_symbol(interpreter, 'RedBook', expected = 1)
    check_symbol(interpreter, 'red_bk', expected = 1)

    print '\n***Testing subsequent dictation of reformatted symbol***\n'

    utterances.append(string.split('new statement red book equals zero'))
    input.append('0\n')
    status.append(1)
    test_say(utterances[-1], input[-1], never_bypass_sr_recog=1)


    print '\n***Testing reformatting of an existing symbol to an other existing symgol ***\n'
    the_mediator.add_symbol('BlueBook', ['blue book'], tentative=0)
    the_mediator.add_symbol('blue_bk', ['blue book'], tentative=0)
    check_symbol(interpreter, 'BlueBook', expected = 1)
    check_symbol(interpreter, 'blue_bk', expected = 1)

    utterances.append(string.split('new statement blue book equals none'))
    input.append('0\n')
    status.append(1)
    test_say(utterances[-1], input[-1], never_bypass_sr_recog=1)

    sym_corrections = {}
    sym_corrections[1] = ('blue book', 'BlueBook', 'blue_bk')
    correct_recent_symbols(instance_name, sym_corrections, user_input = '0\n')

    print '\n***Testing state***\n'

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    print '\n***Checking that both symbols are still there***\n'

    check_symbol(interpreter, 'BlueBook', expected = 1)
    check_symbol(interpreter, 'blue_bk', expected = 1)

    print '\n***Testing subsequent dictation of reformatted symbol***\n'

    utterances.append(string.split('new statement blue book equals zero'))
    input.append('0\n')
    status.append(1)
    test_say(utterances[-1], input[-1], never_bypass_sr_recog=1)

    print '\n***Testing reformatting of a new symbol to a existing one ***\n'
    the_mediator.add_symbol('osym', ['O. symbol'], tentative=0)
    check_symbol(interpreter, 'osym', expected = 1)
    check_symbol(interpreter, 'other_symbol', expected = 0)

    utterances.append(string.split('new statement other symbol equals zero'))
    input.append('0\n')
    status.append(1)
    test_say(utterances[-1], input[-1], never_bypass_sr_recog=1)

    sym_corrections = {}
    sym_corrections[1] = ('other symbol', 'other_symbol', 'osym')
    correct_recent_symbols(instance_name, sym_corrections, user_input = '0\n')

    print '\n***Testing state***\n'

    print '\n## AD: Is there an issue with the warnings below?##\n'

    check_stored_utterances(instance_name, expected = len(utterances))
    check_recent(instance_name, utterances, status)

    print '\n***Checking that tentative new symbol was removed ***\n'

    check_symbol(interpreter, 'osym', expected = 1)
    check_symbol(interpreter, 'other_symbol', expected = 1)

    print '\n***Testing subsequent dictation of reformatted symbol***\n'

    utterances.append(string.split('new statement other symbol equals one'))
    input.append('0\n')
    status.append(1)
    test_say(utterances[-1], input[-1], never_bypass_sr_recog=1)

    print '\n***Testing that symbol reformatting with empty list of reformattings does not cause a crash***\n'
    correct_recent_symbols(instance_name, {}, user_input = '0\n')



add_test('basic_correction', test_basic_correction,
    'Testing basic correction infrastructure with ResMgr.')


##############################################################################
# Testing special purpose wxWindows widget subclasses
##############################################################################

def test_wxWindowsWithHelpers():
   suite = unittest.TestSuite()
   suite.addTest(unittest.makeSuite(wxWindowsWithHelpersTest.wxListCtrlWithHelpersTest,'test'))
   suite.addTest(unittest.makeSuite(wxWindowsWithHelpersTest.wxDialogWithHelpersTest,'test'))
   suite.addTest(unittest.makeSuite(wxWindowsWithHelpersTest.wxButtonWithHelpersTest,'test'))

   unittest.TextTestRunner().run(suite)

add_test('wxWindowsWithHelpers', test_wxWindowsWithHelpers, 'Testing subclasses of wxWindows widgets.')



##############################################################################
# Testing symbol reformatting UI
##############################################################################

def test_reformat_recent_dlg():
   unittest.TextTestRunner(). \
       run(unittest.makeSuite(MediatorConsoleWXTests.ReformatRecentTestCase, 'test'))


add_test('reformat_recent_dlg', test_reformat_recent_dlg,
         'Testing dialog for selecting a symbol to reformat.')

def test_reformat_from_recent_dlg():
   unittest.TextTestRunner(). \
       run(unittest.makeSuite(MediatorConsoleWXTests.ReformatFromRecentTestCase, 'test'))

add_test('reformat_from_recent_dlg', test_reformat_from_recent_dlg,
         'Testing dialog for reformatting a selected symbol.')


##############################################################################
# Test retrieval of recently dictated symbols
##############################################################################

def test_recent_symbol_retrieval():
    testing.init_simulator_regression()
    instance_name = testing.instance_name()
    commands.open_file('blah.py')

    the_mediator = testing.mediator()

    check_recent_symbols(instance_name, "Before dictation")

    utterances = []
    utterances.append(string.split('class some class inherits from some other class'))
    input = ['0\n0\n']
#    status = [1]

    for i in range(len(utterances)):
        test_say(utterances[i], user_input = input[i], never_bypass_sr_recog=1)

    check_recent_symbols(instance_name, "After dictation")

add_test('recent_symbols_retrieval', test_recent_symbol_retrieval,
         'Test retrieval of recently dictated symbols.')


##############################################################################
# Testing set_text
##############################################################################
def test_set_text():

    global small_buff_c, small_buff_py, large_buff_py

    testing.init_simulator_regression()
    the_mediator = testing.mediator()
    instance_name = testing.instance_name()
    editor = the_mediator.editors.app_instance(instance_name)
    commands.open_file(small_buff_py)
    buffer = editor.curr_buffer()
    buffer.set_text('nothing left')
    editor.print_buff_if_necessary()
    buffer.set_text('almost ', start = 0, end = 0)
    editor.print_buff_if_necessary()
    buffer.set_text('body', start = 9, end = 14)
    editor.print_buff_if_necessary()


add_test('set_text', test_set_text,
    'Testing set_text.')

##############################################################################
# Insertion deletion commands
##############################################################################

def test_insert_delete_commands():
   testing.init_simulator_regression()
   commands.open_file('blah.py')
   test_say(['this', 'is', 'a', 'very', 'long', 'variable', 'name', 'but', 'never', 'mind'], user_input="1\n1\n1\n1\n1\n1\n1\n1\n1\n")
   test_say(['back space'])
   test_say(['two\\two', 'times'])
   test_say(['back space 2'])
   test_say(['back space 3'])
   test_say(['back space 4'])
   test_say(['back space 5'])
   mediator = testing.mediator()
   instance_name = testing.instance_name()
   editor = mediator.editor_instance(instance_name)
   editor.set_text('some additional text')
   test_say(['select', 'additional'], never_bypass_sr_recog=1)
   test_say(['back space'])
   editor.set_text('some additional text')
   test_say(['select', 'additional'], never_bypass_sr_recog=1)
   test_say(['back space 2'])

   commands.open_file(large_buff_py)
   commands.goto_line(5)

   test_say(['delete', 'that', 'line'])
   test_say(['do', 'that', 'again'])
   test_say(['delete', 'that', 'line', 'two\\two', 'times'])

   test_say(['select', 'base', 'class'])
   test_say(['delete', 'that'])

   test_say(['select', 'various', 'useful'])
   test_say(['yo', 'copy', 'that'])
   test_say(['yo', 'paste', 'that'])

   test_say(['select', 'safe', 'attribute'])
   test_say(['yo', 'cut', 'that'])
   test_say(['yo', 'paste', 'that'])

   commands.goto_line(5)
   test_say(['yo', 'cut', 'line'])
   test_say(['yo', 'paste', 'that'])

   commands.goto_line(4)
   test_say(['yo', 'copy', 'line'])
   test_say(['yo', 'paste', 'that'])


add_test('insert_delete', test_insert_delete_commands, 'Testing insertion and deletion commands',
         foreground = 1)


##############################################################################
# Testing interaction between user inputs and speech
##############################################################################

def request_that_user_bring_editor_to_foreground():
    delay_secs = 5
    util.bell()
    sys.stderr.write("""The next series of tests require that the client editor be in the foreground.

Please do the following:

- press Enter
- immediatly bring the client editor to the foreground
- do not touch the computer until further notice\n\n

Note that after pressing Enter, you will have %s seconds
to bring the editor to the foreground before the tests
start.

Note also that if a background process brings a window to the
foreground, the test results may be invalidated.

Press Enter now:
>  """ % delay_secs)
    sys.stderr.flush()
    dummy = raw_input()
    time.sleep(delay_secs)


#    dummy = raw_input()


def test_mixed_kbd_and_voice_editing():
    testing.init_simulator_regression()
    the_mediator = testing.mediator()
    commands = testing.namespace()['commands']
    instance_name = testing.instance_name()
    app = the_mediator.editors.app_instance(instance_name)

    kbd_evt_sim = testing.kbd_event_sim_factory(app)

    init_line = 9
    commands.open_file(foreground_py, echo_cmd=1)
    test_cursor_moved_by_kbd(app, commands, kbd_evt_sim, init_line)
    test_selection_set_by_kbd(app, commands, kbd_evt_sim, init_line)
    test_search_for_typed_text(app, commands, kbd_evt_sim, init_line)
    test_select_typed_text_by_voice(app, commands, kbd_evt_sim, init_line)

def test_cursor_moved_by_kbd(app, commands, kbd_evt_sim, init_line):
   commands.goto_line(init_line)
   pos = app.cur_pos()
   commands.goto(pos + 1, echo_cmd=1)
   kbd_evt_sim.move_cursor_by_kbd('Right', 10)
   time.sleep(5)
   commands.say(['hello'], user_input="0\n", echo_cmd=1)

def test_selection_set_by_kbd(app, commands, kbd_evt_sim, init_line):
   commands.goto_line(init_line)
   pos = app.cur_pos()
   commands.goto(pos + 1, echo_cmd=1)
   kbd_evt_sim.set_selection_by_kbd('Right', 10)
   time.sleep(5)
   commands.say(['hello'], user_input="0\n", echo_cmd=1)

def test_search_for_typed_text(app, commands, kbd_evt_sim, init_line):
   commands.goto_line(init_line + 1, echo_cmd = 1)
   kbd_evt_sim.type_text(', hi')

   # Need to give Emacs time to notify the server of the typed text
   time.sleep(5)

   kbd_evt_sim.move_cursor_by_kbd('Left', 5)

   time.sleep(5)
   commands.say(['next', 'comma'], echo_cmd=1)

def test_select_typed_text_by_voice(app, commands, kbd_evt_sim, init_line):
   commands.goto_line(init_line + 1, echo_cmd = 1)
   kbd_evt_sim.type_text(' hello ')

   # Need to give Emacs time to notify the server of the typed text
   time.sleep(1)

# user input shouldn't be necessary, but is useful for testing with
# WaxEditClient where the keyboard input isn't working
   commands.say(['select', 'hello'], never_bypass_sr_recog=1, echo_cmd=1, user_input = "0\n")

add_test('mixed_mode_editing', test_mixed_kbd_and_voice_editing, 'Testing mixed mode (kbd + voice) editing', foreground = 1)

##############################################################################
# Test dictation in split window.
##############################################################################

def test_Emacs_split_window():
    testing.init_simulator_regression()
    commands = testing.namespace()['commands']
    instance_name = testing.instance_name()
    the_mediator = testing.mediator()
# this is useless here, since
    app = the_mediator.editors.app_instance(instance_name)
    if app.app_name != 'emacs':
        print 'This test only works when emacs is the test editor'
        return

    kbd_evt_sim = testing.kbd_event_sim_factory(app)

    commands.open_file(foreground_py, echo_cmd=1)
    commands.say(['dictated', 'in', 'foreground', 'buffer'], user_input="0\n", echo_cmd=1)

    #
    # Split the window in two and display a second buffer
    #
    kbd_evt_sim.type_text('{Esc}xsplit-window-vertically{Enter}')
    kbd_evt_sim.type_text('{Esc}xother-window{Enter}')

    time.sleep(5)

    commands.open_file('buf2.py', echo_cmd=1)
    commands.say(['dictated', 'in', 'buffer', 'two'], user_input="0\n", echo_cmd=1)

    time.sleep(5)

    commands.say(['dictated', 'in', 'buffer', 'two'], user_input="0\n", echo_cmd=1)

    kbd_evt_sim.type_text('{Esc}xdelete-other-window{Enter}')


add_test('emacs_split_window', test_Emacs_split_window, 'Testing dictation into Emacs with two buffers displayed in same window.', foreground = 1)

##############################################################################
# Test normal text dictation.
##############################################################################

def test_normal_text_dictation():

   testing.init_simulator_regression()
   unittest.TextTestRunner(). \
       run(unittest.makeSuite(TextModeTest.TextModeTest, 'test'))


add_test('text_mode', test_normal_text_dictation, 'Test dictation of normal text.', foreground = 1)

##############################################################################
# Number dictation
##############################################################################

def print_block(block, name = 'text'):
    s = name + ': '
    if block is None:
        print s + '(no block)'
    else:
        print s + "range: %d %d, text = %s" % \
        (block.start(), block.end(), repr(block.text))

def test_inserted_text():
    testing.init_simulator_regression()

    editor = testing.editor()

    commands.open_file('blah.py')

    t = 'x = 3'
    print 'inserting %s' % repr(t)
    text = editor.insert(t)
    editor.print_buff()
    print_block(text)

    t = '\n'
    print 'inserting %s' % repr(t)
    text = editor.insert(t)
    editor.print_buff()
    print_block(text)

    code_bef  = 'if '
    code_after = ':\n\t'
    print 'inserting %s, %s' % (repr(code_bef), repr(code_after))
    before, after = editor.insert_indent(code_bef, code_after)
    editor.print_buff()
    print_block(before, 'before cursor')
    print_block(after, 'after_cursor')

    code_bef  = 'x > 2'
    code_after = ''
    print 'inserting %s, %s' % (repr(code_bef), repr(code_after))
    before, after = editor.insert_indent(code_bef, code_after)
    editor.print_buff()
    print_block(before, 'before cursor')
    print_block(after, 'after_cursor')

    editor.search_for('\n *')
    code_bef  = 'multi()'
    code_after = ''
    print 'inserting %s, %s' % (repr(code_bef), repr(code_after))
    before, after = editor.insert_indent(code_bef, code_after)
    editor.print_buff()
    print_block(before, 'before cursor')
    print_block(after, 'after_cursor')

    code_bef  = '\nelif '
    code_after = ':\n\t'
    print 'inserting %s, %s' % (repr(code_bef), repr(code_after))
    before, after = editor.insert_indent(code_bef, code_after)
    editor.print_buff()
    print_block(before, 'before cursor')
    print_block(after, 'after_cursor')

    code_bef  = 'x == 1'
    code_after = ''
    print 'inserting %s, %s' % (repr(code_bef), repr(code_after))
    before, after = editor.insert_indent(code_bef, code_after)
    editor.print_buff()
    print_block(before, 'before cursor')
    print_block(after, 'after_cursor')
    editor.search_for('\n *')
    code_bef  = 'solitaire()'
    code_after = ''
    print 'inserting %s, %s' % (repr(code_bef), repr(code_after))
    before, after = editor.insert_indent(code_bef, code_after)
    editor.print_buff()
    print_block(before, 'before cursor')
    print_block(after, 'after_cursor')

    code_bef  = '\nelse:\n'
    code_after = ''
    print 'inserting %s, %s' % (repr(code_bef), repr(code_after))
    before, after = editor.insert_indent(code_bef, code_after)
    editor.print_buff()
    print_block(before, 'before cursor')
    print_block(after, 'after_cursor')

    code_bef  = 'dual()'
    code_after = ''
    print 'inserting %s, %s' % (repr(code_bef), repr(code_after))
    before, after = editor.insert_indent(code_bef, code_after)
    editor.print_buff()
    print_block(before, 'before cursor')
    print_block(after, 'after_cursor')

add_test('inserted_text', test_inserted_text,
  desc='Test of new reporting system for inserted text')


##############################################################################
# Number dictation
##############################################################################

def test_number_dictation():
   testing.init_simulator_regression()
   app = testing.editor()

   commands.open_file('blah.py')
   commands.say(['23\\twenty-three', '54\\fifty-four', 'comma', '0\\zero', '.\\point', '04\\oh four'], echo_cmd=1)

# Works for DCF (Natspeak 7), but not for Alain (Natspeak 5?)
#   commands.say(['select', '23\\twenty-three', '54\\fifty-four'], never_bypass_sr_recog=1, echo_cmd=1, user_input="0\n")
#   app.print_buff()

# Works for DCF (Natspeak 7) but not for Alain (Natspeak 5?)
#   commands.say(['select', 'twenty', 'three', 'fifty', 'four'], never_bypass_sr_recog=1, echo_cmd=1, user_input="0\n")
#   app.print_buff()

# This works for Alain (Natspeak 5), but not for DCF (Natspeak 7)
#   commands.say(['select', '23', '54'], never_bypass_sr_recog=1, echo_cmd=1, user_input="0\n")
#   app.print_buff()

# (this fails with badWord, because there is no word '2354' in the
# vocabulary - DCF)
#   commands.say(['select', '2354'], never_bypass_sr_recog=1, echo_cmd=1, user_input="0\n")

# This works for Alain
   commands.say(['select', '0\\zero', '.\\point'], never_bypass_sr_recog=1, echo_cmd=1, user_input="0\n")
# This works for Alain
   commands.say(['select', '04\\oh four'], never_bypass_sr_recog=1, echo_cmd=1, user_input="0\n")
# This doesn't work for Alain
#   commands.say(['select', '0\\zero', '.\\point', 'oh', 'four'], never_bypass_sr_recog=1, echo_cmd=1, user_input="0\n")
# But this works for Alain
   commands.say(['select', '0\\zero', '.\\point', '04\\oh four'], never_bypass_sr_recog=1, echo_cmd=1, user_input="0\n")

add_test('number_dictation', test_number_dictation, desc='Test number dictation')




##############################################################################
# Inserting new statements above/below current line
##############################################################################

def test_new_statement():
   testing.init_simulator_regression()

   commands.open_file('blah1.py')

   commands.say(['new', 'statement', 'below'] , user_input="0\n", echo_utterance=1)
   commands.say(['below', 'when', 'empty', 'buffer'], user_input='1\n', echo_utterance=1)

   commands.goto(0, echo_cmd=1)
   commands.say(['new', 'statement', 'below'] , user_input="0\n", echo_utterance=1)
   commands.say(['below', 'when', 'at', 'first',
                 'line', 'of', 'a', 'buffer'], user_input='1\n', echo_utterance=1)

   commands.goto(commands.app.len(), echo_cmd=1)
   commands.say(['new', 'statement', 'below'] , user_input="0\n", echo_utterance=1)
   commands.say(['below', 'when', 'at', 'last',
                 'character', 'of', 'a', 'buffer'], user_input='1\n', echo_utterance=1)

   commands.goto_line(2, echo_cmd=1)
   commands.say(['new', 'statement', 'below'] , user_input="0\n", echo_utterance=1)
   commands.say(['below', 'when', 'on', 'middle',
                 'line', 'of', 'buffer'], user_input='1\n', echo_utterance=1)

   commands.goto_line(3, echo_cmd=1)
   commands.goto_end_of_line()
   commands.say(['new', 'statement', 'below'] , user_input="0\n", echo_utterance=1)
   commands.say(['below', 'when', 'at', 'end', 'of', 'line'],
                 user_input='1\n', echo_utterance=1)

   commands.goto_line(3, echo_cmd=1)
   commands.goto_beginning_of_line()
   commands.say(['new', 'statement', 'below'] , user_input="0\n", echo_utterance=1)
   commands.say(['below', 'when', 'at', 'beginning', 'of', 'line'],
                 user_input='1\n', echo_utterance=1)


   commands.goto(10, echo_cmd=1)
   commands.say(['new', 'statement', 'below'] , user_input="0\n", echo_utterance=1)
   commands.say(['below', 'when', 'at', 'middle',
                 'of', 'a', 'line'], user_input='1\n', echo_utterance=1)


   commands.open_file('blah2.py')

   commands.say(['new', 'statement', 'above'] , user_input="0\n", echo_utterance=1)
   commands.say(['above', 'when', 'empty', 'buffer'], user_input='1\n', echo_utterance=1)

   commands.goto(0, echo_cmd=1)
   commands.say(['new', 'statement', 'above'] , user_input="0\n", echo_utterance=1)
   commands.say(['above', 'when', 'at', 'first',
                 'line', 'of', 'a', 'buffer'], user_input='1\n', echo_utterance=1)

   commands.goto(commands.app.len(), echo_cmd=1)
   commands.say(['new', 'statement', 'above'] , user_input="0\n", echo_utterance=1)
   commands.say(['above', 'when', 'at', 'last',
                 'character', 'of', 'a', 'buffer'], user_input='1\n', echo_utterance=1)

   commands.goto_line(2, echo_cmd=1)
   commands.say(['new', 'statement', 'above'] , user_input="0\n", echo_utterance=1)
   commands.say(['above', 'when', 'on', 'middle',
                 'line', 'of', 'buffer'], user_input='1\n', echo_utterance=1)

   commands.goto_line(3, echo_cmd=1)
   commands.goto_end_of_line()
   commands.say(['new', 'statement', 'above'] , user_input="0\n", echo_utterance=1)
   commands.say(['above', 'when', 'at', 'end', 'of', 'line'],
                 user_input='1\n', echo_utterance=1)

   commands.goto_line(3, echo_cmd=1)
   commands.goto_beginning_of_line()
   commands.say(['new', 'statement', 'above'] , user_input="0\n", echo_utterance=1)
   commands.say(['above', 'when', 'at', 'beginning', 'of', 'line'],
                 user_input='1\n', echo_utterance=1)


   commands.goto(10, echo_cmd=1)
   commands.say(['new', 'statement', 'above'] , user_input="0\n", echo_utterance=1)
   commands.say(['above', 'when', 'at', 'middle',
                 'of', 'a', 'line'], user_input='1\n', echo_utterance=1)

add_test('new_statement', test_new_statement, desc='Test creation of new statements above/below current line')




##############################################################################
# Voice Commands for compiling symbols
##############################################################################

def test_compile_symbols():

   global small_buff_c, small_buff_py, large_buff_py

   testing.init_simulator_regression()
   commands.open_file(small_buff_py)
   commands.clear_symbols()
   print "Before compiling symbols, symbols are:\n"
   commands.print_symbols()
   commands.say(['compile symbols'], never_bypass_sr_recog=1)
   print "After compiling symbols, symbols are:\n"
   commands.print_symbols()

add_test('compile_symbols', test_compile_symbols, 'Testing voice command for compiling symbols')


##############################################################################
# Testing commands that have a special meaning only on a blank line
##############################################################################


def test_blank_line_context():
   testing.init_simulator_regression()

   commands.open_file('blah1.py')

   commands.say(['for', 'do', 'the', 'following'] , user_input="0\n", echo_utterance=1)
   commands.say(['security', 'level', 'equals', 'for', 'your', 'eyes', 'only', 'new', 'statement', 'back', 'indent'] , user_input="1\n1\n", echo_utterance=1)

   commands.say(['while', 'loop', 'one', 'do', 'the', 'following'] , user_input="0\n", echo_utterance=1)
   commands.say(['when', 'equals', 'while', 'processing', 'new', 'statement'] , user_input="1\n1\n", echo_utterance=1)

   commands.say(['if', 'condition', 'do', 'the', 'following'] , user_input="1\n", echo_utterance=1)
   commands.say(['check', 'equals', 'if', 'all', 'right', 'without', 'arguments', 'new', 'statement'] , user_input="1\n1\n", echo_utterance=1)

   commands.say(['back', 'indent', 'back', 'indent'] , user_input="1\n", echo_utterance=1)
   commands.say(['class', 'some', 'class', 'class', 'body'] , user_input="1\n", echo_utterance=1)

   commands.say(['try'] , user_input="1\n", echo_utterance=1)
   commands.say(['flag', 'equals', 'did', 'try', 'new', 'statement'] , user_input="1\n1\n1\n", echo_utterance=1)

   commands.say(['the', 'next', 'ones', 'still', 'present', 'bugs', 'new', 'statement'], user_input="1\n1\n", echo_utterance=1)
   commands.say(['check', 'if', 'was', 'done', 'without', 'arguments', 'new', 'statement'] , user_input="1\n1\n", echo_utterance=1)

add_test('blank_line_context', test_blank_line_context, 'Testing commands that have a special meaning only a a blank line')


##############################################################################
# Testing AppState looking_at method
##############################################################################


def test_looking_at():
   testing.init_simulator_regression()

   try:
      commands.open_file('blah1.py')
      test_word = 'hello'
      commands.say([test_word] , user_input="0\n", echo_utterance=1)
      assert not commands.app.looking_at(test_word), \
             "Thought we were looking at '%s' when we weren't." % test_word
      commands.goto_beginning_of_line()
      assert commands.app.looking_at(test_word), \
             "Thought we weren't looking at '%s' when we were." % test_word
   except Exception, err:
      test_helpers.failed_test_assertion(err)

   commands.goto_end_of_line()

add_test('looking_at', test_looking_at, 'Testing the looking at method.')


##############################################################################
# Testing AppState beginning/end of line
##############################################################################


def test_beginning_end_of_line():
   testing.init_simulator_regression()

   commands.open_file('blah1.py')
   commands.say(['line', 'one', 'new', 'statement', 'line', 'two', 'new', 'statement', 'line', 'three'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.goto_beginning_of_line(echo_cmd=1)
   commands.goto_beginning_of_line(echo_cmd=1)
   commands.move_relative(-1, echo_cmd=1)
   commands.goto_beginning_of_line(echo_cmd=1)
   commands.move_relative(3, echo_cmd=1)
   commands.goto_beginning_of_line(echo_cmd=1)
   commands.goto(0, echo_cmd=1)
   commands.goto_beginning_of_line(echo_cmd=1)

   commands.open_file('blah2.py')
   commands.say(['line', 'one', 'new', 'statement', 'line', 'two', 'new', 'statement', 'line', 'three'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.goto(0, echo_cmd=1)
   commands.goto_end_of_line(echo_cmd=1)
   commands.goto_end_of_line(echo_cmd=1)
   commands.move_relative(1, echo_cmd=1)
   commands.goto_end_of_line(echo_cmd=1)
   commands.move_relative(-3, echo_cmd=1)
   commands.goto_end_of_line(echo_cmd=1)
   commands.goto(commands.app.len(), echo_cmd=1)
   commands.goto_end_of_line(echo_cmd=1)

add_test('beg_end_of_line', test_beginning_end_of_line, 'Testing methods for going to the beginning or end of a line.')

##############################################################################
# Testing explicit indentation
##############################################################################


def test_explicit_indentation():
   testing.init_simulator_regression()

   commands.open_file('blah1.py')
   commands.say(['if', 'condition', 'do', 'the', 'following'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['back', 'indent'], user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['index', 'equals', 'one', 'do', 'the', 'following'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['indent'], user_input="1\n1\n1\n", echo_utterance=1)

   # Try backindenting from middle of the line
   commands.move_relative(-3, echo_cmd=1)
   commands.say(['back', 'indent'], user_input="1\n1\n1\n", echo_utterance=1)

   # Try indenting forward from middle of the line
   commands.move_relative(3, echo_cmd=1)
   commands.say(['indent'], user_input="1\n1\n1\n", echo_utterance=1)


add_test('explicit_indent', test_explicit_indentation, 'Testing explicit indentation.')


##############################################################################
# Testing dictation of standard function calls
##############################################################################

def test_standard_function_call():
   testing.init_simulator_regression()

   commands.open_file('blah1.py')

   # These ones should be interpreted as calls
   commands.say(['absolute', 'value', 'with', 'arguments', 'one'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['new', 'statement'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['absolute', 'value', 'of', 'one'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['new', 'statement'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['absolute', 'value', 'without', 'arguments'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['new', 'statement'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['some', 'function', 'with', 'argument', 'some', 'argument'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['new', 'statement'] , user_input="1\n1\n1\n", echo_utterance=1)

   # These ones should be interpreted as new symbols
   commands.say(['absolute', 'value'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['new', 'statement'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['some', 'function', 'of', 'some', 'argument'] , user_input="1\n1\n1\n", echo_utterance=1)




add_test('std_func_calls', test_standard_function_call, 'Testing CSCs for calling standard functions.')




##############################################################################
# Testing navigation within buffer with page up/down etc.
##############################################################################

def test_navigation_within_buffer():
   testing.init_simulator_regression()
   unittest.TextTestRunner(). \
       run(unittest.makeSuite(NavigationWithinBufferTest.NavigationWithinBufferTest, 'test'))

add_test('navigation_within_buffer', test_navigation_within_buffer,
         desc='unit testing navigation commands like page up/down etc.')




##############################################################################
# Testing special cases for the symbol matching algorithm
##############################################################################

def test_sym_matching():
   testing.init_simulator_regression()
   testing.mediator().interp.add_symbol('symbolWithUnmatchableExplicitSpokenForm', user_supplied_spoken_forms=['purple bunny'])
   testing.mediator().interp.add_symbol('filepath')
   testing.mediator().interp.add_symbol('dpath')
   testing.mediator().interp.add_symbol('rannum')
   testing.mediator().interp.add_symbol('intfmt')
   testing.mediator().interp.add_symbol('TTCorp')
   testing.mediator().interp.add_symbol('GrnRab')
   testing.mediator().interp.add_symbol('EdSim')
   testing.mediator().interp.add_symbol('__rab__')
   testing.mediator().interp.add_symbol('datap')

   commands.open_file('blah1.py')

   # Should type EdSim
   commands.say(['editor', 'simulator'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['new', 'statement'] , user_input="1\n1\n1\n", echo_utterance=1)

   # Should type symbolWithUnmatchableExplicitSpokenForm
   commands.say(['purple', 'bunny'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['new', 'statement'] , user_input="1\n1\n1\n", echo_utterance=1)

   # Should NOT type acos (aco is not a prefix of application, and it's too short
   # to be allowed to be concatenated with another word from the pseudo-symbol)
   commands.say(['application', 'state'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['new', 'statement'] , user_input="1\n1\n1\n", echo_utterance=1)

   # Should type filepath (file and path are prefixes of file and path, so it's OK to concatenate it with
   # path)
   commands.say(['file', 'path'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['new', 'statement'] , user_input="1\n1\n1\n", echo_utterance=1)

   # Should type dpath (d and path are prefixes of directory and path)
   commands.say(['directory', 'path'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['new', 'statement'] , user_input="1\n1\n1\n", echo_utterance=1)

   # Should type TTCorp (T, T and Corp are prefixes of Toronto, Transit and Corporation
   # (but doesn't at the moment)
   commands.say(['Toronto', 'transit', 'corporation'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['new', 'statement'] , user_input="1\n1\n1\n", echo_utterance=1)

   # Should type rannum (ran and num are prefixes of random and num, so can concatenate
   # them)
   commands.say(['random', 'number'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['new', 'statement'] , user_input="1\n1\n1\n", echo_utterance=1)

   # Should type GrnRab (eventhough Grn is not a suffix of green and it's too short
   # to be allowed to concatenate, there is a change of case, and therefore GnRab
   # does not count as a concatenation)
   commands.say(['green', 'rabbit'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['new', 'statement'] , user_input="1\n1\n1\n", echo_utterance=1)

   # Should NOT type intfmt (fmt is not a prefix of format and it's too short
   # to be allowed to concatenate)
   commands.say(['integer', 'format'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['new', 'statement'] , user_input="1\n1\n1\n", echo_utterance=1)

   # Should NOT type __rab__ (cause it's too short if you don't count the _s)
   commands.say(['rabbit'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['new', 'statement'] , user_input="1\n1\n1\n", echo_utterance=1)

   # Should type datap (data and p are prefixes of data and processing, so it's
   # ok to concatenate them... this case is to make sure that we process the last
   # term correctly)
   commands.say(['data', 'processing'] , user_input="1\n1\n1\n", echo_utterance=1)
   commands.say(['new', 'statement'] , user_input="1\n1\n1\n", echo_utterance=1)

add_test('symbol_matching', test_sym_matching, desc='Test special cases for the symbol matching algorithm.')

##############################################################################
# Switching to another buffer by voice
##############################################################################

def test_emacs_switch_buffer():

    unittest.TextTestRunner(). \
       run(unittest.makeSuite(SwitchBufferTest.SwitchBufferTest, 'test'))
    return


add_test('switch_buffer', test_emacs_switch_buffer,
         foreground=1, desc='Switching to an other buffer in Emacs.')

##############################################################################
# Saving a buffer by voice in Emacs
##############################################################################

def test_emacs_save_buffer():
    testing.init_simulator_regression()
    file_name = os.path.join(vc_globals.tmp, 'dummy.py')
    try:
       os.path.remove(file_name)
    except Exception:
       pass
    commands.open_file(file_name)
    commands.say(['class', 'dummy'], echo_cmd=1)
    commands.say(['yo', 'save', 'buffer'], echo_cmd=1)
    commands.open_file(file_name)

#add_test('save_buffer', test_emacs_save_buffer,
#         foreground=1, desc='Saving a buffer by voice in Emacs.')


##############################################################################
# Checking language of a buffer
##############################################################################

def test_language_name_for_file(file_name):
    testing.init_simulator_regression()
    commands.open_file(file_name)
    print "\nLanguage for this file is: %s\n" % commands.app.curr_buffer().language_name()


def test_language_name():
    test_language_name_for_file("dummy.py")
    test_language_name_for_file("dummy.c")
    test_language_name_for_file("dummy.h")
    test_language_name_for_file("dummy.cpp")
    test_language_name_for_file("fileWithMultipleDots.InItsName.py")


add_test('language_name', test_language_name,
         desc='Testing language name of a buffer.')



##############################################################################
# Sending a large message to the client
##############################################################################

def test_large_messages():
    testing.init_simulator_regression()
    commands.open_file('tmp.py')
    commands.app.set_text(generate_string_of_approx_length(1024))
    commands.app.print_buff()


def generate_string_of_approx_length(str_len):
   the_string = ""
   line_len = 1
   len_to_now = 0
   while len_to_now < str_len:
      the_string = "%s%s" % (the_string, line_len)
      len_to_now = len_to_now + 1
      line_len = line_len + 1
      if line_len >= 9:
         line_len = 1
         the_string = "%s\n" % the_string
         len_to_now = len_to_now + 1
   return the_string


add_test('large_messages', test_large_messages, desc='Send a message that has more than 1024 character (length of a message chunk)')

##############################################################################
# test for profiling startup/configuration
##############################################################################

def test_profile_config():
   testing.init_simulator_regression()

add_test('profile_config', test_profile_config,
    desc='profiling configuration')

##############################################################################
# Use this to create temporary tests
##############################################################################

##add_unittest('Temp', desc='Temp test less than greater than less-than greater-than')

def test_temporary():
    testing.init_simulator_regression()

    commands.open_file('blah1.py')

    commands.say(['backslash'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['back', 'slash'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)
    commands.say(['\\\\backslash'], user_input='2\n2\n2\n2\n2\n2\n2\n', echo_utterance=1)



#add_test('temp', test_temporary, desc='temporary test')

##############################################################################
# Alain Desilets uses this test suite to write reminder to himself.
##############################################################################

class AlainReminderTest(unittest.TestCase):
    def __init__(self, name):
        unittest.TestCase.__init__(self, name)

    def test_check_reminders(self):
        pass
#        self.fail_("reminder here.")


def test_alain_reminder():
    unittest.TextTestRunner(). \
    run(unittest.makeSuite(AlainRemindersTest, 'test'))




##############################################################################
# Define test suites:
##############################################################################

# here are some sample suites
#
# Note that 'all', 'foreground', and 'background' are pre-defined and
# are therefore reserved names.  Also note that suite names must not
# match any existing test names.

sort_tests()

define_suite(name = 'fabfour', tests = ['SymDict', 'automatic_abbreviations',
    'mediator_console', 'python'])

define_suite(name = 'mediator_console_dlgs',
             tests = ['reformat_recent_dlg', 'reformat_from_recent_dlg',
                      'wxWindowsWithHelpers'])

# tests starting with 'CmdInterp' and ending with 'SymDict')
define_suite_by_range(name = 'few_early', first = 'CmdInterp', last = 'SymDict')

# tests up to (and including) 'SymDict')
define_suite_by_range(name = 'thru_SymDict', last = 'SymDict')

# tests starting with 'select_pseudocode' and going through the last
define_suite_by_range(name = 'from_select_pseudo', first = 'select_pseudocode')

# tests starting with 'python' and going through the last
define_suite_by_range(name = 'from_python', first = 'python')

# tests starting with 'python' and going through the last
define_suite_by_range(name = 'from_blank', first = 'blank_line_context')
