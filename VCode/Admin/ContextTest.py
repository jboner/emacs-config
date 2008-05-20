from debug import trace
import cont_gen
import VoiceCodeRootTest
from vc_globals import *
from cont_gen import *

# language context instances:
for lang in all_languages:
   exec('cont%s = Context("%s")'% (lang.capitalize(), lang))
contAnyLanguage = Context(all_languages)
contCStyleLanguage = Context(c_style_languages)


class ContextTest(VoiceCodeRootTest.VoiceCodeRootTest):
    """Test the different context things, more in ContextTest

    and its subclasses.

    """   
    def __init__(self, name):
        VoiceCodeRootTest.VoiceCodeRootTest.__init__(self, name)
      
    def setUp(self):
        self._init_simulator_regression()
        
        # use names like below as global varialbe in eg vc_config
        # use them here as instance variables
        for lang in all_languages:
            exec('self.cont%s = ContLanguage("%s")'% (lang.capitalize(), lang))
        self.contAnyLanguage = ContLanguage(all_languages)
        self.contCStyleLanguage = ContLanguage(c_style_languages)
        self.contAny = ContAny()
        
        
##########################################################
# Documentation tests
#
# These tests illustrate how to use the class.
##########################################################
      
    def test_This_is_how_you_create_a_Context(self):
        contPython = cont_gen.Context("python")
        contAny = ContAny()
        
    def test_A_ContAny_applies_always(self):

        self._open_empty_test_file('temp.py')
        self.assert_(self.contAny.applies(self._app()), 
                     "Context ContAny should have applied because always")

        self._open_empty_test_file('temp.c')
        self.assert_(self.contAny.applies(self._app()), 
                     "Context ContAny should have applied because always")

    def test_Context_equivalence_keys(self):

        # equivalence_key() gives a string, identifying the Context:
        expected = "Language: python"
        self.assert_equal(expected, self.contPython.equivalence_key(),
                              "contPython instance does not produce expected equivalence key")

        expected = "Language: any"
        self.assert_equal(expected, self.contAnyLanguage.equivalence_key(),
                              "contAnyLanguage instance does not produce expected equivalence key")

        expected = "Any"
        self.assert_equal(expected, self.contAny.equivalence_key(),
                              "contAny instance does not isproduce expected equivalence key")


##########################################################
# Unit tests
#
# These tests check the internal workings of the class.
##########################################################

    def test_language_contexts_are_instances_of_correct_classes(self):

        for cont in (self.contAnyLanguage, self.contCStyleLanguage, self.contC, \
                     self.contPython, self.contPerl):
            self.assert_(isinstance(cont, Context),
                               "should be instance of Context class")
            self.assert_(isinstance(cont, ContLanguage),
                               "should be instance of ContLanguage class")
        self.assert_(isinstance(self.contAny, Context),
                           "should be instance of Context class")
        self.assert_(isinstance(self.contAny, ContAny),
                           "should be instance of ContAny class")
        self.failIf(isinstance(self.contAny, ContLanguage),
                           "should NOT be instance of ContLanguage class")
      

    def test_Context_of_ContAny_check_for_context_conflicts(self):
        
         self.failIf(self.contAny.conflicts_with(self.contPython),
                     'The two contexts should not conflict because on different branches')

    def test_Context_of_ContBlankLine_check_for_context_conflicts(self):
        
         self.failIf(self.contAny.conflicts_with(self.contPython),
                     'The two contexts should not conflict because on different branches')

         # instances of same class:
         cont_blank_line_c_style = ContBlankLine(c_style_languages)
         cont_blank_line_python = ContBlankLine('python')
         cont_blank_line_c = ContBlankLine('C')
         cont_blank_line_any = ContBlankLine(all_languages)

         # overlapping:
         self.assert_(cont_blank_line_c_style.conflicts_with(cont_blank_line_c), \
                      'The two contexts should conflict because equal class and overlapping languages' )
         self.assert_(cont_blank_line_c.conflicts_with(cont_blank_line_c_style), \
                      'The two contexts should conflict because equal class and overlapping languages' )
         self.assert_(cont_blank_line_c.conflicts_with(cont_blank_line_c), \
                      'The two contexts should conflict because they are equal' )
        
         self.assert_(cont_blank_line_any.conflicts_with(cont_blank_line_c), \
                      'The two contexts should conflict because equal class and overlapping languages' )
         self.assert_(cont_blank_line_c.conflicts_with(cont_blank_line_any), \
                      'The two contexts should conflict because equal class and overlapping languages' )

         # not overlapping:
         self.failIf(cont_blank_line_c.conflicts_with(cont_blank_line_python), \
                     'The two contexts should not conflict because they have distinct languages')
         self.failIf(cont_blank_line_python.conflicts_with(cont_blank_line_c), \
                     'The two contexts should not conflict because they have distinct languages')
         self.failIf(cont_blank_line_c_style.conflicts_with(cont_blank_line_python), \
                     'The two contexts should not conflict because they have distinct languages')
         self.failIf(cont_blank_line_python.conflicts_with(cont_blank_line_c_style), \
                     'The two contexts should not conflict because they have distinct languages')

    def test_Context_of_ContLanguage_check_for_context_conflicts(self):
        
         # overlapping:
         self.assert_(self.contC.conflicts_with(self.contCStyleLanguage), \
                      'The two contexts should conflict because equal class and overlapping languages' )
         self.assert_(self.contCStyleLanguage.conflicts_with(self.contC), \
                      'The two contexts should conflict because equal class and overlapping languages' )
         self.assert_(self.contC.conflicts_with(self.contC), \
                      'The two contexts should conflict because they are equal' )
        
         self.assert_(self.contAnyLanguage.conflicts_with(self.contC), \
                      'The two contexts should conflict because equal class and overlapping languages' )
         self.assert_(self.contC.conflicts_with(self.contAnyLanguage), \
                      'The two contexts should conflict because equal class and overlapping languages' )

         # not overlapping:
         self.failIf(self.contC.conflicts_with(self.contPython), \
                     'The two contexts should not conflict because they have distinct languages')
         self.failIf(self.contPython.conflicts_with(self.contC), \
                     'The two contexts should not conflict because they have distinct languages')
         self.failIf(self.contCStyleLanguage.conflicts_with(self.contPython), \
                     'The two contexts should not conflict because they have distinct languages')
         self.failIf(self.contPython.conflicts_with(self.contCStyleLanguage), \
                     'The two contexts should not conflict because they have distinct languages')
         

    def test_Context_of_Context_and_subclass_check_for_context_conflicts(self):

         cont_blank_line_c = ContBlankLine('C')
         self.failIf(self.contC.conflicts_with(cont_blank_line_c), \
                     'Contexts should not conflict, because they have different scope')
         self.failIf(cont_blank_line_c.conflicts_with(self.contC), \
                     'Contexts should not conflict, because they have different scope')

    
###############################################################
# Assertions.
# 
# Use these methods to check the state of the class.
###############################################################


    def test_Subclass_of_ContLanguage_with_buffer_scope_context_conflicts(self):

        class ContLanguageSubclass(ContLanguage):
            def scope(self):
                return 'buffer'
            def equivalence_key(self):
                return super(ContLanguageSubclass, self).equivalence_key("TestSubclass")
            def overlaps_with(self, other):
                """should never be called, because is subclass of contPython and contC against which we check"""
                raise Exception("overlaps_with should never be called here")
            
        contL = ContLanguageSubclass('python')
        self.assert_(contL.conflicts_with(self.contPython), "bufferscope subclass of ContLanguage should conflict with same language ContLanguage instance")
        self.failIf(contL.conflicts_with(self.contC), "bufferscope subclass of ContLanguage should NOT conflict with other language ContLanguage instance")

        
        self.assert_(self.contPython.conflicts_with(contL), "bufferscope subclass of ContLanguage should conflict with same language ContLanguage instance")
        self.failIf(self.contC.conflicts_with(contL), "bufferscope subclass of ContLanguage should NOT conflict with other language ContLanguage instance")

    def test_ContLanguage_instances_languages_are_sorted(self):
         contMixed1 = ContLanguage( ('python', 'perl') )
         contMixed2 = ContLanguage( ('perl', 'python') )
         self.assert_equal(contMixed2, contMixed1, \
                           "language order of ContLanguage instances does not matter")
         self.assert_equal(contMixed1.equivalence_key(), contMixed2.equivalence_key(), "the equivalence_keys should also be the same")

                                    

    def test_Context_should_fail_with_invalid_input(self):
         self.assertRaises(ValueError, ContLanguage, "unknown")
         self.assertRaises(TypeError, ContLanguage, ['c', 'perl'])
         self.assertRaises(DeprecationError, ContPy)
         self.assertRaises(DeprecationError, ContC)
         self.assertRaises(DeprecationError, ContPerl)
                                                 
