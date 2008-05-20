import debug
import VoiceCodeRootTest
from vc_globals import *
import vc_globals

import os, glob, shutil
import regression
import itertools
import pprint
from copy import copy
from CmdInterp import AliasMeaning, CmdInterp, LSAlias, LSAliasSet, CSCmdSet, CmdSet
from CSCmd import CSCmd
from cont_gen import *
import WhatCanISay
import util
from actions_gen import gen_parens_pair, ActionInsertNewClause, ActionInsert
from actions_C_Cpp import c_else
from config_helpers import *


# language context instances:
for lang in all_languages:
   exec('cont%s = ContLanguage("%s")'% (lang.capitalize(), lang))
contAnyLanguage = ContLanguage(all_languages)
contCStyleLanguage = ContLanguage(c_style_languages)
contAny = ContAny()


# test data:
expected_languages = ['python', 'C', 'perl'] # for the default test case
expected_languages.sort()


lsa_multiply_spoken_forms = ['multiply by', 'times']
lsa_multiply_meanings  = dict.fromkeys(expected_languages, ' * ')
lsa_not_spoken_forms = ['not']
lsa_not_duplicate_spoken_forms = ['n o t']
lsa_not_meanings  = dict(python='not', C="!", perl="!")

csc_with_arguments_spoken_forms = ['with arguments']
csc_with_arguments_meanings = {all_languages: gen_parens_pair}

csc_with_arguments_docstring = 'giving the parens after a function call, position inside'
csc_else_spoken_forms = ['else']
csc_else_duplicate_spoken_forms = ['else duplicate']
csc_python_else_meanings ={ContBlankLine('python'): ActionInsertNewClause('($|\n)',
                                                     code_bef = 'else on blank line',
                                                     code_after = '',
                                                     where = -1),
                    contPython: ActionInsert("else on non blank line")}
csc_c_else_meanings = {contC: c_else}


csc_python_else_docstring = 'else clause only python'
csc_c_else_docstring = 'else clause only c'
csc_equals_spoken_forms = ['equals']
csc_equals_meanings ={ContPyInsideArguments(): ActionInsert("="),
                       ContAny(): ActionInsert(' = ')}
csc_equals_docstring = 'equal sign'

# lsa in case csc does not apply:
lsa_equals_spoken_forms = ['equals']
lsa_equals_meanings = {('python', 'C'):  ' = '}

expected_index = \
   {'python': {'else': [[{'action': 'no docstring available',
                       'doc': 'else clause only python',
                       'equiv': 'BlankLine: python',
                       'scope': 'immediate',
                       'setdescription': 'description of CSCS',
                       'setname': 'csc commands'},
                      {'action': "Inserts 'else on non blank line^' in current buffer",
                       'doc': 'else clause only python',
                       'equiv': 'Language: python',
                       'scope': 'buffer',
                       'setdescription': 'description of CSCS',
                       'setname': 'csc commands'}]],
            'else duplicate': [[{'action': 'no docstring available',
                                 'doc': 'else clause only python',
                                 'equiv': 'BlankLine: python',
                                 'scope': 'immediate',
                                 'setdescription': 'description of CSCS',
                                 'setname': 'csc commands'},
                                {'action': "Inserts 'else on non blank line^' in current buffer",
                                 'doc': 'else clause only python',
                                 'equiv': 'Language: python',
                                 'scope': 'buffer',
                                 'setdescription': 'description of CSCS',
                                 'setname': 'csc commands'}]],
            'equals': [[{'action': "Inserts '=^' in current buffer",
                         'doc': 'equal sign',
                         'equiv': 'ContPyInsideArguments: python',
                         'scope': 'immediate',
                         'setdescription': 'description of CSCS',
                         'setname': 'csc commands'},
                        {'action': "Inserts ' = ^' in current buffer",
                         'doc': 'equal sign',
                         'equiv': 'Any',
                         'scope': 'global',
                         'setdescription': 'description of CSCS',
                         'setname': 'csc commands'}],
                       {'name': 'equals lsa',
                        'new_symbol': None,
                        'setdescription': 'description of LSAS',
                        'setname': 'lsa/ commands',
                        'spacing': 0,
                        'written_form': ' = '}],
            'multiply by': [{'name': 'multiply',
                             'new_symbol': None,
                             'setdescription': 'description of LSAS',
                             'setname': 'lsa/ commands',
                             'spacing': 0,
                             'written_form': ' * '}],
            'n o t': [{'name': 'not',
                       'new_symbol': None,
                       'setdescription': 'description of LSAS',
                       'setname': 'lsa/ commands',
                       'spacing': 0,
                       'written_form': 'not'}],
            'not': [{'name': 'not',
                     'new_symbol': None,
                     'setdescription': 'description of LSAS',
                     'setname': 'lsa/ commands',
                     'spacing': 0,
                     'written_form': 'not'}],
            'times': [{'name': 'multiply',
                       'new_symbol': None,
                       'setdescription': 'description of LSAS',
                       'setname': 'lsa/ commands',
                       'spacing': 0,
                       'written_form': ' * '}],
            'with arguments': [[{'action': 'Insert parens and puts cursor in between',
                                 'doc': 'giving the parens after a function call, position inside',
                                 'equiv': 'Language: any',
                                 'scope': 'buffer',
                                 'setdescription': 'description of CSCS',
                                 'setname': 'csc commands'}]]}}

# files testing (required apart from generated html files:
required_non_html_files = ['vc.css', 'vcodeuser.jpg', 'waveform.gif']


class WhatCanISayTest(VoiceCodeRootTest.VoiceCodeRootTest):
    """tests of WhatCanISay functionality

    testing the actual html output files is a bit fake:
    if the folder WhatCanISayTestResults in VCODE_HOME\Data\Benchmark, or
    subfolders like "python", "C", "perl" (and possibly additional languages) do not exist,
    the test run creates them and uses them for comparison with future testing. So mainly for
    the developers of these functions.

    If you are working on WhatCanISay, it is pretty safe to remove above folders as long as you
    test the results in your webbrowser afterwards.

    The test websites are created in  VCODE_HOME\Data\Tmp\language folders(see vc_config)

the test date have a duplicate entry "else" and "else duplicate", which should conflict at define time
the "equals" csc and lsa should (for python) show up with
    3 entries normal, and with 1 entry for inside or outside arguments list

    """
   
    def __init__(self, name):
      VoiceCodeRootTest.VoiceCodeRootTest.__init__(self, name)
      
    def setUp(self):
        print '\n=======setting up WhatCanISay test instance'
        self.wciSay = WhatCanISay.WhatCanISay()
        self.interp = CmdInterp()

        # lsa set:
        lsas = LSAliasSet("lsa/ commands", description="description of LSAS")
        lsas.add_lsa(LSAlias(lsa_multiply_spoken_forms, lsa_multiply_meanings, name="multiply"))
        lsas.add_lsa(LSAlias(lsa_not_spoken_forms, lsa_not_meanings, name="not"))
        lsas.add_lsa(LSAlias(lsa_not_duplicate_spoken_forms, lsa_not_meanings, name="not"))
        lsas.add_lsa(LSAlias(lsa_equals_spoken_forms, lsa_equals_meanings, name="equals lsa"))
        self.interp.add_lsa_set(lsas)

        # csc set:
        cscs = CSCmdSet('csc commands', description='description of CSCS')
        cscs2 = CSCmdSet('csc commands too', description='description duplicates of CSCS')
        cscs.add_csc(CSCmd(spoken_forms=csc_with_arguments_spoken_forms,
                           meanings=csc_with_arguments_meanings,
                           docstring=csc_with_arguments_docstring))

        # here the tricky one: else only for python:        
        cscs.add_csc(CSCmd(spoken_forms=csc_else_spoken_forms,
                           meanings=csc_python_else_meanings,
                           docstring=csc_python_else_docstring))
        # else duplicate, only for python:
        cscs.add_csc(CSCmd(spoken_forms=csc_else_duplicate_spoken_forms,
                           meanings=csc_python_else_meanings,
                           docstring=csc_python_else_docstring))
        # csc_c_else_meanings only for c, group should be : csc commands too!

        # and inanother set: else for c! should come in set cscs commands too!!!        
        cscs2.add_csc(CSCmd(spoken_forms=csc_else_spoken_forms,
                           meanings=csc_c_else_meanings,
                           docstring=csc_c_else_docstring))
        cscs.add_csc(CSCmd(spoken_forms=csc_equals_spoken_forms,
                           meanings=csc_equals_meanings,
                           docstring=csc_equals_docstring))
        self.interp.add_csc_set(cscs)
        self.interp.add_csc_set(cscs2)
        

        # punctuation:
        punc = SinglePunctuation(name = 'standard punctuation')
        punc.add('%', ['percent-sign'])
        punc.create(self.interp)
        
        self.wciSay.load_commands_from_interpreter(self._app(), self.interp, 'python')

      
##########################################################
# Documentation tests
#
# These tests illustrate how to use the class.
##########################################################
      

    def test_This_is_how_you_create_a_WhatCanISay_instance(self):
        wciSay = WhatCanISay.WhatCanISay()
        interp = CmdInterp()
        # load one lsa and one csc:
        interp.add_csc(CSCmd(["equals"], meanings={contAny: ActionInsert("====")}, name="equals csc"))
        interp.add_lsa(LSAlias(["plus"], meanings={all_languages: " + "}, name="plus sign"))
        wciSay.load_commands_from_interpreter(self._app(), interp, 'C')
        

    def test_This_is_how_you_create_the_commands_for_showing(self):
        wciSay = WhatCanISay.WhatCanISay()
        interp = CmdInterp()
        # load one lsa and one csc:
        interp.add_csc(CSCmd(["equals"], meanings={contAny: ActionInsert("====")}, name="equals csc"))
        interp.add_lsa(LSAlias(["plus"], meanings={all_languages: " + "}, name="plus sign"))
        wciSay.load_commands_from_interpreter(self._app(), interp, 'C')
        wciSay.create_cmds()

    def test_This_is_how_to_create_the_pages(self):
        """in order to automatically show the pages
 
        hard to test, except by eye...
        note: possibly disable when doing all tests automatically
        """
        self.wciSay.create_cmds()
        self.wciSay.create_html_pages()

    def test_This_is_how_to_create_and_show_the_pages(self):
        """in order to automatically show the pages
 
        hard to test, except by eye...
        note: possibly disable when doing all tests automatically
        """
##        self.wciSay.create_cmds()
##        self.wciSay.create_html_pages()
##        self.wciSay.show_cmds()
        pass
        
        
##########################################################
# Unit tests lsa and general commands
#
# These tests check the internal workings of the class.
##########################################################

    def test_the_index_of_WhatCanISay_default(self):

        # all_lang = None, curr_context = None, curr_lang = 'python' in this test case
        self.assert_equal(expected_index, self.wciSay.index, "test index of WhatCanISay (default) is not as expected")
        self.wciSay.create_cmds()
        expected_top_menu =   {'python': 'python_overview.html'}
        expected_left_menu =        {'python': {'csc commands': 'python_csccommands.html',
                                               'lsa/ commands': 'python_lsacommands.html'}}
        expected_top_menu_keys = ['python']
        expected_left_menu_keys = {'python': ['csc commands', 'lsa/ commands']}
        self.assert_equal(expected_top_menu, self.wciSay.top_menu, "top menu of WhatCanISay (default) not as expected")
        self.assert_equal(expected_left_menu, self.wciSay.left_menu, "left menu of WhatCanISay (default) not as expected")
        self.assert_equal(expected_top_menu_keys, self.wciSay.top_menu_keys, "top menu keys of WhatCanISay (default) not as expected")

        self.assert_equal(expected_left_menu_keys, self.wciSay.left_menu_keys,
                         "left menu keys of WhatCanISay (default) not as expected")

    def test_the_index_of_WhatCanISay_all_lang(self):

        # all_lang = 1, curr_context = None, curr_lang = 'python' in this test case
        self.wciSay.load_commands_from_interpreter(self._app(), self.interp, 'python', all_lang=1)
        expected_index_keys_all_lang = list(all_languages)
        actual_keys = self.wciSay.index.keys()
        actual_keys.sort()
        self.assert_equal(expected_index_keys_all_lang, actual_keys, "test index of WhatCanISay .(all_lang) has not expected keys")
##     self.assert_equal(expected_boilerplate, self.wciSay.boilerplate, \
##                          "test boilerplate of WhatCanISay (all_lang) is not as expected")
        # move on to the commands:
        self.wciSay.create_cmds()
        # changes with new languages:
        expected_top_menu =       {'C': 'c_overview.html',
 'java': 'java_overview.html',
 'javascript': 'javascript_overview.html',
 'perl': 'perl_overview.html',
 'php': 'php_overview.html',
 'python': 'python_overview.html'}
        
        # changes with new languages:
        expected_left_menu = \
   {'C': {'csc commands': 'c_csccommands.html',
       'csc commands too': 'c_csccommandstoo.html',
       'lsa/ commands': 'c_lsacommands.html',
       'standard punctuation': 'c_standardpunctuation.html',
       'standard punctuation navigation': 'c_standardpunctuationnavigation.html'},
 'java': {'csc commands': 'java_csccommands.html',
          'standard punctuation': 'java_standardpunctuation.html',
          'standard punctuation navigation': 'java_standardpunctuationnavigation.html'},
 'javascript': {'csc commands': 'javascript_csccommands.html',
                'standard punctuation': 'javascript_standardpunctuation.html',
                'standard punctuation navigation': 'javascript_standardpunctuationnavigation.html'},
 'perl': {'csc commands': 'perl_csccommands.html',
          'lsa/ commands': 'perl_lsacommands.html',
          'standard punctuation': 'perl_standardpunctuation.html',
          'standard punctuation navigation': 'perl_standardpunctuationnavigation.html'},
 'php': {'csc commands': 'php_csccommands.html',
         'standard punctuation': 'php_standardpunctuation.html',
         'standard punctuation navigation': 'php_standardpunctuationnavigation.html'},
 'python': {'csc commands': 'python_csccommands.html',
            'lsa/ commands': 'python_lsacommands.html',
            'standard punctuation': 'python_standardpunctuation.html',
            'standard punctuation navigation': 'python_standardpunctuationnavigation.html'}}

        # changes with mew languages:
        expected_top_menu_keys = ['C', 'java', 'javascript', 'perl', 'php', 'python']
        expected_left_menu_keys =   \
            {'C': ['csc commands', 'csc commands too', 'lsa/ commands',
                   'standard punctuation', 'standard punctuation navigation'],
             'java': ['csc commands', 'standard punctuation', 'standard punctuation navigation'],
             'python': ['csc commands', 'lsa/ commands', 'standard punctuation',
                        'standard punctuation navigation'],
             'javascript': ['csc commands', 'standard punctuation', 'standard punctuation navigation'],
             'perl': ['csc commands', 'lsa/ commands', 'standard punctuation',
                      'standard punctuation navigation'],
             'php': ['csc commands', 'standard punctuation', 'standard punctuation navigation']}
        
        self.assert_equal(expected_top_menu, self.wciSay.top_menu, "top menu of WhatCanISay (all_lang) not as expected")
        self.assert_equal(expected_left_menu, self.wciSay.left_menu, "left menu of WhatCanISay (all_lang) not as expected")
        self.assert_equal(expected_top_menu_keys, self.wciSay.top_menu_keys, "top menu keys of WhatCanISay (all_lang) not as expected")
        self.assert_equal(expected_left_menu_keys, self.wciSay.left_menu_keys,
                         "left menu keys of WhatCanISay (all_lang) not as expected")


    def test_the_index_of_WhatCanISay_curr_context(self):

        self._open_empty_test_file('temp.py')
        self.wciSay.load_commands_from_interpreter(self._app(), self.interp, 'python', curr_context=1)
        # on to the commands:
        self.wciSay.create_cmds()
        expected_top_menu =   {'python': 'python_overview.html'}
        expected_top_menu_keys = ['python']
        expected_left_menu =        {'python': {'csc commands': 'python_csccommands.html',
            'lsa/ commands': 'python_lsacommands.html'}}
        expected_left_menu_keys =   {'python': ['csc commands', 'lsa/ commands']}
        self.assert_equal(expected_top_menu, self.wciSay.top_menu, "top menu of WhatCanISay (curr_context) not as expected")
        self.assert_equal(expected_left_menu, self.wciSay.left_menu, "left menu of WhatCanISay (curr_context) not as expected")
        self.assert_equal(expected_top_menu_keys, self.wciSay.top_menu_keys, " menu keys of WhatCanISay (curr_context) not as expected")
        self.assert_equal(expected_left_menu_keys, self.wciSay.left_menu_keys, "left menu keys of WhatCanISay (curr_context) not as expected")
        

    def test_the_index_of_simple_csc_and_an_lsa_definition(self):
        wciSay = WhatCanISay.WhatCanISay()
        interp = CmdInterp()
        # do one csc and one lsa:
        interp.add_csc(CSCmd(["equals"], meanings={contAny: ActionInsert("====")}, name="equals csc"))
        interp.add_lsa(LSAlias(["plus"], meanings={all_languages: " + "}, name="plus sign"))
        wciSay.load_commands_from_interpreter(self._app(), interp, 'C')
        expected = \
   {'C': {'equals': [[{'action': "Inserts '====^' in current buffer",
                    'doc': None,
                    'equiv': 'Any',
                    'scope': 'global',
                    'setdescription': 'no description',
                    'setname': 'cscs'}]],
       'plus': [{'description': 'no description',
                 'name': 'plus sign',
                 'new_symbol': None,
                 'setname': 'lsas',
                 'spacing': 0,
                 'written_form': ' + '}]}}
        self.assert_equal(expected, wciSay.index, "index of one CSC and one LSA command is not as expected")

    def test_the_index_of_a_cmdset_with_an_lsa_and_a_csc_in_it(self):
        wciSay = WhatCanISay.WhatCanISay()
        interp = CmdInterp()
        # do one csc and one lsa:
        cmds = CmdSet("commands set")
        cmds.add_lsa(LSAlias(["plus"], meanings={all_languages: " + "}, name="plus sign"))
        cmds.add_csc(CSCmd(["equals"], meanings={contAny: ActionInsert("====")}, name="equals csc"))
        
        interp.add_cmd_set(cmds)
        wciSay.load_commands_from_interpreter(self._app(), interp, 'C')
        expected = \
    {'C': {'equals': [[{'action': "Inserts '====^' in current buffer",
                    'doc': None,
                    'equiv': 'Any',
                    'scope': 'global',
                    'setdescription': None,
                    'setname': 'commands set'}]],
       'plus': [{'name': 'plus sign',
                 'new_symbol': None,
                 'setdescription': 'no description',
                 'setname': 'commands set',
                 'spacing': 0,
                 'written_form': ' + '}]}}
        self.assert_equal(expected, wciSay.index, "index of one CSC and one LSA command is not as expected")


    def test_the_index_of_c_else_with_different_csc_set_name_as_python_else(self):
        wciSay = WhatCanISay.WhatCanISay()
        interp = CmdInterp()
        # do a csc set for python and a csc set for c
        # note: for try all went well in C, except for the setname
        cscs1 = CSCmdSet('try command python', description='description of CSCS')
        cscs2 = CSCmdSet('try command C', description='description duplicates of CSCS')
        cscs2.add_csc(CSCmd(spoken_forms=csc_else_spoken_forms,
                           meanings=csc_c_else_meanings,
                           docstring=csc_c_else_docstring))
        cscs1.add_csc(CSCmd(spoken_forms=csc_else_spoken_forms,
                           meanings=csc_python_else_meanings,
                           docstring=csc_python_else_docstring))
        interp.add_csc_set(cscs2)
        interp.add_csc_set(cscs1)
        interp.add_lsa(LSAlias(["plus"], meanings={all_languages: " + "}, name="plus sign"))

        wciSay.load_commands_from_interpreter(self._app(), interp, 'C')
        expected = \
   {'C': {'else': [[{'action': 'else clause of a C conditional',
                  'doc': 'else clause only c',
                  'equiv': 'Language: C',
                  'scope': 'buffer',
                  'setdescription': 'description duplicates of CSCS',
                  'setname': 'try command C'}]],
       'plus': [{'description': 'no description',
                 'name': 'plus sign',
                 'new_symbol': None,
                 'setname': 'lsas',
                 'spacing': 0,
                 'written_form': ' + '}]}}                 
        self.assert_equal(expected, wciSay.index, "index of else csc set in C different from expected")

        wciSay.load_commands_from_interpreter(self._app(), interp, 'python')
        expected = \
   {'python': {'else': [[{'action': 'no docstring available',
                       'doc': 'else clause only python',
                       'equiv': 'BlankLine: python',
                       'scope': 'immediate',
                       'setdescription': 'description of CSCS',
                       'setname': 'try command python'},
                      {'action': "Inserts 'else on non blank line^' in current buffer",
                       'doc': 'else clause only python',
                       'equiv': 'Language: python',
                       'scope': 'buffer',
                       'setdescription': 'description of CSCS',
                       'setname': 'try command python'}]],
            'plus': [{'description': 'no description',
                      'name': 'plus sign',
                      'new_symbol': None,
                      'setname': 'lsas',
                      'spacing': 0,
                      'written_form': ' + '}]}}
        self.assert_equal(expected, wciSay.index, "index of else csc set in python different from expected")


 
    def test_control_standard_files(self):
        """Controls the existence of some standard files"""
        folder= self.wciSay.html_folder
        self.assert_(os.path.isdir(folder),
            'WhatCanISay does not have a valid folder %s'%folder)
        for f in required_non_html_files:
           F = os.path.join(folder, f)
           self.assert_(os.path.isfile(F),
                'file "%s" is missing (and is required for the WhatCanISay website (%s)).'% (f, F))

        
    def test_resulting_websites(self):
        """check if all the files are equal"""
        for lang in expected_languages:
            print '\ntesting default wcisay website for language: %s'% lang
            self.do_test_resulting_website(lang, all_lang=None, curr_context=None, comment=None)
  
            if lang in ('C', 'python'):
                
                ext = dict(C='c', python='py')[lang]
                self._open_empty_test_file('temp.%s'% ext)
                print '\ntesting the current context website "blank" for language: %s'% lang
                self.do_test_resulting_website(lang, all_lang=None, curr_context=1, comment="blank")
                self._insert_in_active_buffer("now line is (inside arguments list")
                print '\ntesting the current context website "filled" for language: %s'% lang
                self.do_test_resulting_website(lang, curr_context=1, comment="filled")

        # all languages produce (should) same result with all_lang flag on:
        self.do_test_resulting_website('C', all_lang=1)
                
                


    def do_test_resulting_website(self, lang, all_lang=None, curr_context=None, comment=None):
        """make the website for these paramters and check against previous result.

         if curr_context = 1, a context should have been setup first, and comment gives a clue
         to the correct folder to check against.

         only test html files         

         if no html files present in folder, copy result into the folder and assume the result is correct

        """         
        html_folder= self.wciSay.html_folder

        html_files = glob.glob(os.path.join(html_folder, "*.html"))
        for f in html_files:
            os.remove(f)
        
        self.wciSay.load_commands_from_interpreter(self._app(), self.interp, lang,
                                                    all_lang=all_lang, curr_context=curr_context)
        self.wciSay.create_cmds()
        self.wciSay.create_html_pages()

        test_home = vc_globals.wcisay_test_folder
        self.assert_(os.path.isdir(test_home), "No valid folder for testing the resulting websites")
        if all_lang:
            test_folder = os.path.join(test_home, "all_lang")
        elif curr_context:
            test_folder = os.path.join(test_home, lang + "_curr_context_" + comment or "")
        else:
            test_folder = os.path.join(test_home, lang)
           
        if os.path.isdir(test_folder):
##            print 'using test folder: %s'% test_folder
            old_files = glob.glob(os.path.join(test_folder , "*.html"))
            if not old_files:
                print 'empty folder for language %s assume correct results, copy to %s'% \
                      (lang, util.within_VCode(test_folder))
                self.copy_html_files(html_folder, test_folder)
                return
        else:
            print 'no test folder yet for language %s, assume correct results, copy to %s'% \
                  (lang, test_folder)
            self.copy_html_files(html_folder, test_folder)
            return
        
        self.assert_equal_html_files(test_folder, html_folder,
                                      'WhatCanISay website of language %s (with all_lang: %s, curr_context: %s, comment: %s'%
                                      (lang, all_lang, curr_context, comment))

    def test_context_applies_for_lang(self):
        self.assert_(self.wciSay.context_applies_for_lang('python', contPython), 
                      "ContPy should applie for langage python")
        self.failIf(self.wciSay.context_applies_for_lang('C', contPython), 
                      "ContPy should not apply for langage C")

        self.assert_(self.wciSay.context_applies_for_lang('C', contC), 
                      "ContC should apply for langage C")                      
        self.failIf(self.wciSay.context_applies_for_lang('python', contC), 
                      "ContC should not apply for langage python")
                      
        self.assert_(self.wciSay.context_applies_for_lang('python', ContAny()), 
                      "ContAny should apply for langage python")
        self.assert_(self.wciSay.context_applies_for_lang('C', ContAny()), 
                      "ContAny should apply for langage C")         
        

###############################################################
# Assertions and utility function testing:
#
###############################################################
    def assert_equal_html_files(self, expected_folder, actual_folder, mess):
        """test the equality of the html files"""
        expected_list = glob.glob(os.path.join(expected_folder, '*.html'))
        actual_list = glob.glob(os.path.join(actual_folder, '*.html'))
        expected_list.sort()
        actual_list.sort()
        self.assert_equal(len(expected_list), len(actual_list), mess + '\n' + \
                       "number of expected html files not equal to actual\nExpected: %s\nActual: %s"%
                         (expected_list, actual_list))
        for e, a in zip(expected_list, actual_list):
           file_e = os.path.basename(e)
           file_a = os.path.basename(a)
           
           self.assert_equal(file_e, file_a, "filenames are not equal:\n%s and\n%s"%
                                (file_e, file_a))
           
           self.assert_equal_files(e, a, "files are not equal:\n%s and\n%s"%
                                (e, a))

  
    def assert_equal_files(self, expected_file, actual_file, mess):
        for i, k, l in itertools.izip(itertools.count(1),
                                      open(expected_file),
                                      open(actual_file)):
         
           if k.find('class="copyright"') >= 0 and \
               l.find('class="copyright"') >= 0:
                continue  # dates can differ, is expected and accepted
           self.assert_equal(k,l,mess + '\nthe two files------------\n%s\nand\n %s should have been equal\nThey differ in line %s'%
                               (expected_file, actual_file, i))


    def copy_html_files(self, src_dir, dest_dir):
        """copy only html files from src to dest"""
        if os.path.isdir(dest_dir):
           shutil.rmtree(dest_dir)
        os.makedirs(dest_dir)
        self.assert_(os.path.isdir(dest_dir), 'could not make empty folder %s'% dest_dir)

        html_files = glob.glob(src_dir + os.sep + '*.html')
        for src in html_files:
           dest = src.replace(src_dir, dest_dir)
           shutil.copyfile(src, dest)
           
    def test_WhatCanISay_with_all_lang_and_curr_context_should_fail(self):
        self.assertRaises(ValueError, self.wciSay.load_commands_from_interpreter,
                         self._app(), self.interp, 'python', 1, 1)


        
        

##def _test():
## do not remove, for quick dirty testing QH
##    reload(WhatCanISay)
##    wciSay = WhatCanISay.WhatCanISay()
##    wciSay.curr_lang = 'python'
##    wciSay.all_lang = None
##    wciSay.curr_context = None
####    wciSay.index = expected_index
##    wciSay.language = 'python'
##    wciSay.create_cmds()
##    wciSay.create_html_pages()

##if __name__ == "__main__":
##    _test()
