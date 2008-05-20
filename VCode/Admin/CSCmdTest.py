import debug
import VoiceCodeRootTest
from vc_globals import *
import os, glob, shutil
import regression
import itertools
from pprint import pprint
from copy import copy
from CmdInterp import AliasMeaning, CmdInterp, LSAlias, CSCmdSet
from CSCmd import CSCmd
from cont_gen import *
from actions_gen import *
from actions_C_Cpp import *


# language context instances:
for lang in all_languages:
    exec('cont%s = ContLanguage("%s")'% (lang.capitalize(), lang))
contAnyLanguage = ContLanguage(all_languages)
contCStyleLanguage = ContLanguage(c_style_languages)

# test data:
expected_languages = ['C', 'perl', 'python']

csc_with_arguments_spoken_forms = ['with arguments', 'function of']
csc_with_arguments_meanings = {all_languages: gen_parens_pair}

csc_with_arguments_docstring = 'giving the parens after a function call, position inside'
csc_else_spoken_forms = ['else']
csc_else_meanings ={contPython: ActionInsertNewClause('($|\n)',
                                                                     code_bef = 'else:\n\t',
                                                                     code_after = '',
                                                                     where = -1),
                          c_style_languages: c_else}
csc_else_docstring = 'else clause'
csc_equals_spoken_forms = ['equals', 'assign value']

csc_equals_meanings ={all_languages: ActionInsert("="),
                             ContAny(): ActionInsert(' = ')}
csc_equals_docstring = 'equal sign'

expected_csc_index =  {\
     'python':\
         {'function of': [(contPython, gen_parens_pair)],
          'with arguments': [(contPython, gen_parens_pair)],
          'assign value': [(ContAny(), ActionInsert(" = ")),
                                 (ContPyInsideArguments(), ActionInsert("="))],
          'equals': [(ContAny(), ActionInsert(" = ")),
                         (ContPyInsideArguments(), ActionInsert("="))],
          'else': [(contPython, ActionInsertNewClause('($|\n)',
                                                                     code_bef = 'else:\n\t',
                                                                     code_after = ''))]},
     'C':\
         {'function of': [(contC, gen_parens_pair)],
          'with arguments':[(contC, gen_parens_pair)],
          'assign value': [(ContAny(),  ActionInsert(" = "))],
          'equals': [(ContAny(), ActionInsert(" = "))],
          'else': [(c_style_languages, c_else)]},
    'perl':\
         {'function of': [(contPerl, gen_parens_pair)],      
          'with arguments': [(contPerl, gen_parens_pair)],
          'assign value': [(ContAny(), ActionInsert(" = "))],
          'equals': [(ContAny(), ActionInsert(" = "))],
          'else': [(c_style_languages, c_else)]}}



class CSCmdTest(VoiceCodeRootTest.VoiceCodeRootTest):
    """tests of CSCmd and CSCmdDict

    """
    
    def __init__(self, name):
        VoiceCodeRootTest.VoiceCodeRootTest.__init__(self, name)
        
    def setUp(self):
         self.interp = CmdInterp()
         self.interp.add_csc(CSCmd(spoken_forms=csc_with_arguments_spoken_forms,
                                    meanings=csc_with_arguments_meanings,
                                    docstring=csc_with_arguments_docstring))
         self.interp.add_csc(CSCmd(spoken_forms=csc_else_spoken_forms,
                                    meanings=csc_else_meanings,
                                    docstring=csc_else_docstring))
         self.interp.add_csc(CSCmd(spoken_forms=csc_equals_spoken_forms,
                                    meanings=csc_equals_meanings,
                                    docstring=csc_equals_docstring))


        
##########################################################
# Documentation tests
#
# These tests illustrate how to use the class.
##########################################################
        

    def test_This_is_how_you_create_a_CSCmd_instance(self):
         command1 = CSCmd(spoken_forms=['hello'],
                         meanings = {'python': ActionInsert('hello python'),
                                         'C': ActionInsert('hello C')})
         command2 = CSCmd(spoken_forms=['hello'],
                         meanings = {contPython: ActionInsert('hello python'),
                                         contC: ActionInsert('hello C')})
         command3 = CSCmd(spoken_forms=['hello'],
                         meanings = {ContLanguage('python'): ActionInsert('hello python'),
                                         ContLanguage('C'): ActionInsert('hello C')})
         self.assert_equal(command2, command1, \
                                 "Csc commands 2 and 1 should have been the same with different ways to define")
         self.assert_equal(command3, command1, \
                                 "Csc commands 3 and 1 should have been the same with different ways to define")


    def test_This_is_how_you_collect_all_the_CSC_commands_and_get_info(self):
         interp = CmdInterp()
         contAny = ContAny()
         actionHello = ActionInsert("hello")
         contBlankLine = ContBlankLine()
         actionThere = ActionInsert("there")
         actionOtherwise = ActionInsert("there otherwise")
         interp.add_csc(CSCmd(spoken_forms=['hello'],
                                     meanings={contAny: actionHello},
                                     docstring="hello"))
         
         interp.add_csc(CSCmd(spoken_forms=['there'],
                                     meanings={contBlankLine: actionThere,
                                               contAny: actionOtherwise},
                                     docstring="there on blankline or otherwise"))
         
         wTrie = interp.commands
         for spoken, cscmd_list in wTrie.items():
             
             if spoken == ['hello']:
                 cscmd_list_expected = [{'action': "Inserts 'hello^' in current buffer",
                                         'doc': 'hello',
                                         'equiv': 'Any',
                                         'scope': 'global',
                                         'setdescription': 'no description',
                                         'setname': 'cscs'}]
             elif spoken == ['there']:
                 cscmd_list_expected = [{'action': "Inserts 'there^' in current buffer",
                                         'doc': 'there on blankline or otherwise',
                                         'equiv': 'BlankLine: any',
                                         'scope': 'immediate',
                                         'setdescription': 'no description',
                                         'setname': 'cscs'},
                                        {'action': "Inserts 'there otherwise^' in current buffer",
                                         'doc': 'there on blankline or otherwise',
                                         'equiv': 'Any',
                                         'scope': 'global',
                                         'setdescription': 'no description',
                                         'setname': 'cscs'}]
             visible_list = cscmd_list.get_info()
             self.assert_equal(cscmd_list_expected, visible_list,
                               'wTrie CSCmdList of meanings with spoken form "%s" is not as expected'% repr(spoken))
             
    def test_This_is_how_you_enter_and_get_info_through_CSCmdSet(self):
         interp = CmdInterp()
         actionHello = ActionInsert("csc hello")
         contAny = ContAny()
         cscs = CSCmdSet('csc demo set', description='description of csc demo set')
         cscs.add_csc(CSCmd(spoken_forms=['hello'],
                                     meanings={contAny: actionHello},
                                     docstring="hello in csc set"))
         interp.add_csc_set(cscs)
         wTrie = interp.commands
         for spoken, cscmd_list in wTrie.items():
             
             if spoken == ['hello']:
                 cscmd_list_expected = [{'action': "Inserts 'csc hello^' in current buffer",
                                         'doc': 'hello in csc set',
                                         'equiv': 'Any',
                                         'scope': 'global',
                                         'setdescription': 'description of csc demo set',
                                         'setname': 'csc demo set'}]
             else:
                 self.fail("should not come here, testing error, should have exactly 1 spoken form")
             visible_list = cscmd_list.get_info()
             self.assert_equal(cscmd_list_expected, visible_list,
                               'wTrie CSCmdList of meanings with spoken form "%s" is not as expected'% repr(spoken))
             break
         else:
             self.fail("no spoken forms in test, should not come here, testing error, should have 1 spoken form")


##########################################################
# Unit tests lsa and general commands
#
# These tests check the internal workings of the class.
##########################################################



    def test_Test_correct_order_of_CSCmdList1(self):
         interp = CmdInterp()
         actionHello = ActionInsert("csc hello")
         actionBlankLine = ActionInsert("csc blank")
         actionBeforeArguments = ActionInsert("csc before arguments")
         contBlankLine = ContBlankLine()
         contAny = ContAny()
         cscs = CSCmdSet('csc demo set', description='description of csc demo set')
         # test the sorting of the csc :
         cscs.add_csc(CSCmd(spoken_forms=['hello'],
                                     meanings={ContPyBeforeArguments(): actionBeforeArguments,
                                               contAny: actionHello,
                                               contBlankLine: actionBlankLine},
                                    docstring="hello in csc set testlist1"))                                 
         interp.add_csc_set(cscs)
         wTrie = interp.commands
         for spoken, cscmd_list in wTrie.items():
             
             if spoken == ['hello']:
                 cscmd_list_expected = \
   [{'action': "Inserts 'csc blank^' in current buffer",
  'doc': 'hello in csc set testlist1',
  'equiv': 'BlankLine: any',
  'scope': 'immediate',
  'setdescription': 'description of csc demo set',
  'setname': 'csc demo set'},
 {'action': "Inserts 'csc before arguments^' in current buffer",
  'doc': 'hello in csc set testlist1',
  'equiv': 'ContPyBeforeArguments: python',
  'scope': 'immediate',
  'setdescription': 'description of csc demo set',
  'setname': 'csc demo set'},
 {'action': "Inserts 'csc hello^' in current buffer",
  'doc': 'hello in csc set testlist1',
  'equiv': 'Any',
  'scope': 'global',
  'setdescription': 'description of csc demo set',
  'setname': 'csc demo set'}]

             else:
                 self.fail("should not come here, testing error, should have exactly 1 spoken form")
             visible_list = cscmd_list.get_info()
             self.assert_equal(cscmd_list_expected, visible_list,
                               'wTrie CSCmdList of meanings with spoken form "%s" is not as expected'% repr(spoken))
             break
         else:
             self.fail("no spoken forms in test, should not come here, testing error, should have 1 spoken form")
 
    def test_Test_correct_order_of_CSCmdList2(self):
         interp = CmdInterp()
         actionHello = ActionInsert("csc inverted")
         actionBlankLine = ActionInsert("csc blank")
         # test the sorting of the csc inverted:
         contAny = ContAny()
         contBlankLine = ContBlankLine()
         cscs = CSCmdSet('csc demo set', description='description of csc demo set')
         cscs.add_csc(CSCmd(spoken_forms=['inverted'],
                                     meanings={contBlankLine: actionBlankLine,
                                               contAny: actionHello},
                                    docstring="hello in csc set testlist2"))                                 
                                 
         interp.add_csc_set(cscs)
         wTrie = interp.commands
         for spoken, cscmd_list in wTrie.items():
             
             if spoken == ['inverted']:
                 cscmd_list_expected =    [\
 {'action': "Inserts 'csc blank^' in current buffer",
  'doc': 'hello in csc set testlist2',
  'equiv': 'BlankLine: any',
  'scope': 'immediate',
  'setdescription': 'description of csc demo set',
  'setname': 'csc demo set'},
 {'action': "Inserts 'csc inverted^' in current buffer",
  'doc': 'hello in csc set testlist2',
  'equiv': 'Any',
  'scope': 'global',
  'setdescription': 'description of csc demo set',
  'setname': 'csc demo set'}]
             else:
                 self.fail("should not come here, testing error, should have exactly 1 spoken form")
             visible_list = cscmd_list.get_info()
             self.assert_equal(cscmd_list_expected, visible_list,
                               'wTrie CSCmdList of meanings with spoken form "%s" is not as expected'% repr(spoken))
             break
         else:
             self.fail("no spoken forms in test, should not come here, testing error, should have 1 spoken form")
        # see ContextTest.py
        

    def test_Test_correct_order_of_CSCmdList_permutations_test(self):
        """Add meanings in different orders, placing in CSCmdList should be sorted right away"""
        actionHello = ActionInsert("csc hello")
        actionBlankLine = ActionInsert("csc blank")
        actionBeforeArguments = ActionInsert("csc before arguments")
        contBlankLine = ContBlankLine()
        contAny = ContAny()
        CAList = [(ContPyBeforeArguments(), actionBeforeArguments),
                  (contAny, actionHello), 
                  (contBlankLine, actionBlankLine)]

        for permutation in [(1,2,0), (2,0,1), (0,2,1), (1,0,2), (2,1,0), (0,1,2)]:
             interp = CmdInterp()
             CAListPermuted = [CAList[i] for i in permutation]
             cscs = CSCmdSet('csc demo set', description='description of csc demo set')
             # test the sorting of the csc. order of dict not important, always returning same CSCmdList:
             meanings = dict()
             for (c,a) in CAListPermuted:
                 meanings[c] = a
             csc = CSCmd(spoken_forms=['hello'],
                                     meanings=meanings,
                                     docstring="hello in csc test permutations")
             cscs.add_csc(csc)
             interp.add_csc_set(cscs)
             wTrie = interp.commands
             for spoken, cscmd_list in wTrie.items():
                 
                 if spoken == ['hello']:
                     cscmd_list_expected = \
  [{'action': "Inserts 'csc blank^' in current buffer",
  'doc': 'hello in csc test permutations',
  'equiv': 'BlankLine: any',
  'scope': 'immediate',
  'setdescription': 'description of csc demo set',
  'setname': 'csc demo set'},
 {'action': "Inserts 'csc before arguments^' in current buffer",
  'doc': 'hello in csc test permutations',
  'equiv': 'ContPyBeforeArguments: python',
  'scope': 'immediate',
  'setdescription': 'description of csc demo set',
  'setname': 'csc demo set'},
 {'action': "Inserts 'csc hello^' in current buffer",
  'doc': 'hello in csc test permutations',
  'equiv': 'Any',
  'scope': 'global',
  'setdescription': 'description of csc demo set',
  'setname': 'csc demo set'}]

                 else:
                     self.fail("should not come here, testing error, should have exactly 1 spoken form")
                 visible_list = cscmd_list.get_info()
                 self.assert_equal(cscmd_list_expected, visible_list,
                                   'wTrie CSCmdList of meanings with spoken form "%s" (permutation: %s) is not as expected'% \
                                   (repr(spoken), permutation))
                 break
             else:
                 self.fail("no spoken forms in test, should not come here, testing error, should have 1 spoken form")



    def test_Test_conflicing_context_instances(self):
        pass
        # see ContextTest.py
