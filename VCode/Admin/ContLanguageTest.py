from debug import trace
import cont_gen
import VoiceCodeRootTest
from vc_globals import *
from cont_gen import *

# language context instances:
for lang in all_languages:
   exec('cont%s = ContLanguage("%s")'% (lang.capitalize(), lang))
contAnyLanguage = ContLanguage(all_languages)
contCStyleLanguage = ContLanguage(c_style_languages)


class ContLanguageTest(VoiceCodeRootTest.VoiceCodeRootTest):
    """Test the different possibilities to test for ContLanguage instance


    and its subclasses.

    Note: we got rid of the ContPy, ContC etc subclasses. They are instances directly now

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
        
##########################################################
# Documentation tests
#
# These tests illustrate how to use the class.
##########################################################
      
    def test_This_is_how_you_create_a_ContLanguage(self):
        # call with 1 language as string:
        contPython = cont_gen.ContLanguage("python")
        self.assert_equal(self.contPython, contPython,
                          "Context instances should be equal, same language")
        # call with language as tuple of languages:
        contAnyLanguage = ContLanguage(all_languages)
        self.assert_equal(self.contAnyLanguage, contAnyLanguage,
                          "Context instances should be equal, all languages")
        
    def test_A_ContLanguage_applies_only_if_the_active_buffer_is_in_that_language(self):

        self._open_empty_test_file('temp.py')
        self.assert_(self.contPython.applies(self._app()), 
                     "Context language='python' should have applied because in python file")

        self._open_empty_test_file('temp.c')
        self.failIf(self.contPython.applies(self._app()), 
                     "Context should NOT have applied because the file was in the wrong language.")

    def test_ContLanguage_equivalence_keys(self):

        # equivalence_key() gives a string, identifying the Context:
        expected = "Language: python"
        self.assert_equal(expected, self.contPython.equivalence_key(),
                              "contPython instance does not produce expected equivalence key")

        expected = "Language: any"
        self.assert_equal(expected, self.contAnyLanguage.equivalence_key(),
                              "contAnyLanguage instance does not produce expected equivalence key")


        # going on:
        for lang in all_languages:
            context = ContLanguage(lang)
            expected = "Language: %s"% lang 
            self.assert_equal(expected, context.equivalence_key(),
                              "single language ContLanguage instance does not produce expected equivalence key")

        expected = "Language: %s"% '|'.join(c_style_languages)
        self.assert_equal(expected, self.contCStyleLanguage.equivalence_key(),
                              "contCStyleLanguage instance with c_style_languages does not produce expected equivalence key")

##########################################################
# Unit tests
#
# These tests check the internal workings of the class.
##########################################################
    def test_ContLanguage_try_all_possible_languages(self):

        # try all known languages:
        self._open_empty_test_file('temp.py')
        for lang in all_languages:
            context = ContLanguage(language=lang)
            if lang == 'python':
                self.assert_(context.applies(self._app()), 
                     "Context language='python' should have applied because in python file")
            else:
                self.failIf(context.applies(self._app()), 
                     "Context language='%s' should have failed because in python file"% lang)

        # try all_languages in once:
        self._open_empty_test_file('temp.c')
        self.assert_(self.contAnyLanguage.applies(self._app()), 
                      'Context language=all_languages should have applied because in "C" file')

        # trying set of languages:
        self._open_empty_test_file('temp.c')
        self.assert_(self.contCStyleLanguage.applies(self._app()), 
                     'contCStyleLanguage (language="%s") should have applied because in "C" file'% \
                     repr(c_style_languages))
        self._open_empty_test_file('temp.py')
        self.failIf(contCStyleLanguage.applies(self._app()), 
                     'contCStyleLanguage (language="%s") should have failed in python file'
                     ' because it is not a c_style_language'% repr(c_style_languages))
        
    def test_ContLanguage_should_fail_with_invalid_input(self):

        self.assertRaises(ValueError, ContLanguage, "unknown")
        self.assertRaises(TypeError, ContLanguage, ['C', 'perl'])
                                                 
    
###############################################################
# Assertions.
# 
# Use these methods to check the state of the class.
###############################################################
