import debug
import VoiceCodeRootTest
import SymDict

class SymDictTest(VoiceCodeRootTest.VoiceCodeRootTest):
   def __init__(self, name):
      VoiceCodeRootTest.VoiceCodeRootTest.__init__(self, name)
      
   def setUp(self):
       self._init_simulator_regression()
       self.sym_dict = self._symbol_dictionary()

##########################################################
# Documentation tests
#
# These tests illustrate how to use the class.
##########################################################

   def ___test_reminder(self):
       pass
       self.fail("remember to reactivate all other tests in SymDictTest")
      
   def test_This_is_how_you_create_a_SymDict(self):
       interp = SymDict.SymDict()
       
   def test_This_is_how_you_add_a_symbol_to_the_SymDict(self):
       self.sym_dict.add_symbol('ThisIsASymbol')
       
   def test_This_is_how_you_match_the_HEAD_of_a_naturally_spoken_phrase_to_symbols_in_a_SymDict(self):
       sample_spoken_phrase = ['this', 'is', 'a', 'spoken', 'phrase']
       matches = self.sym_dict.match_head(sample_spoken_phrase)
       list_of_matching_symbols = matches[0]
       list_of_remaining_unmatched_words_in_spoken_phrase = matches[1]
       are_these_exact_matches = matches[2]
       
   def test_This_is_how_you_match_a_WHOLE_naturally_spoken_phrase_to_symbols_in_a_SymDict(self):
       self.sym_dict.add_symbol('KnownSymb')
       self.sym_dict.add_symbol('kwn_sm')
       self.sym_dict.add_symbol('ks')
    
       matches = self.sym_dict.match_pseudo_symbol('known symbol')
       good_matches = matches[0]
       weak_matches = matches[1]
       forbidden_matches = matches[2]
       self.assert_equal([(0.51734539969834092, 'KnownSymb')], 
                         good_matches, epsilon=0.01,
                         mess="Good matches were wrong for pseudo-symbol")
       self.assert_equal([(0.36538461538461553, 'kwn_sm'), (0.048185603807257449, 'ks')], weak_matches, 
                         mess="Weak matches were wrong for pseudo-symbol")
       self.assert_equal([], forbidden_matches, 
                         mess="Forbidden matches were wrong for pseudo-symbol")

   def test_When_matching_HEAD_of_a_spoken_phrase_to_a_symbol_you_can_be_more_or_less_demanding_for_fuzzy_matches(self):
       self.sym_dict.add_symbol('KnownSymb')
       self.sym_dict.add_symbol('kwn_sm')
       self.sym_dict.add_symbol('ks')

       matches = self.sym_dict.match_head(
                                ['known', 'symbol'], 
                                use_match_threshold=0.4)
       list_of_matching_symbols = matches[0]
    
       self.assert_equal(['KnownSymb'], 
                         list_of_matching_symbols, 
                         mess="Matching symbols for head of phrase were wrong in case with high match threshold")

       matches = self.sym_dict.match_head(
                               ['known', 'symbol'], 
                               use_match_threshold=0.2)
       good_matches = matches[0]
       self.assert_equal(['KnownSymb','kwn_sm'], 
                         good_matches,
                         mess="Matching symbols for head of phrase were wrong in case with low match threshold")

       
   def test_When_matching_a_WHOLE_spoken_phrase_to_a_symbol_you_can_be_more_or_less_demanding_for_fuzzy_matches(self):
       self.sym_dict.add_symbol('KnownSymb')
       self.sym_dict.add_symbol('kwn_sm')
       self.sym_dict.add_symbol('ks')
    
       matches = self.sym_dict.match_pseudo_symbol('known symbol', use_match_threshold=0.4)
       good_matches = matches[0]
       self.assert_equal([(0.51734539969834092, 'KnownSymb')], 
                         good_matches, epsilon=0.01,
                         mess="Good matches were wrong for high match threshold")

       matches = self.sym_dict.match_pseudo_symbol('known symbol', use_match_threshold=0.2)
       good_matches = matches[0]
       self.assert_equal([(0.51734539969834092, 'KnownSymb'), 
                          (0.36538461538461553, 'kwn_sm')], 
                         good_matches, epsilon=0.01,
                         mess="Good matches were wrong for low match threshold")
       

              

##########################################################
# Unit tests
#
# These tests check the internal workings of the class.
##########################################################

   def test_match_pseudo_symbol(self):
       # Testing match_pseudo_symbol() under varied conditions
       self.assert__match_pseudo_symbol__returns('', ([], [], []),
                "Matching pseudo-symbol failed for empty pseudo-symbol.")
       self.assert__match_pseudo_symbol__returns(
                'known symbol', 
                ([], [], []),
                "Matching pseudo-symbol failed for empty pseudo-symbol.")


      

   def test_match_head(self):
       self.sym_dict.add_symbol('ThisIsASmb')
       self.sym_dict.add_symbol('ThisSymbolDoesNotContainAbbreviations')
       
       self.assert_head_matches_symbol(phrase=['this', 'is', 'a', 'symbol'],
                                  expected_matches=(['ThisIsASmb'], [], False))
       
       self.assert_head_matches_symbol(phrase=['this', 'is', 'a', 'symbol', 'however', 'this', 'is', 'not'],
                                  expected_matches=(['ThisIsASmb'], ['however', 'this', 'is', 'not'], False))

       self.assert_head_matches_symbol(phrase=['this', 'symbol', 'does', 'not', 'contain', 'abbreviations'],
                                  expected_matches=(['ThisSymbolDoesNotContainAbbreviations'], [], True),
                                  mess="Failed trying to match a symbol exactly.")

       
   def test_fuzzy_match_head(self):
       self.sym_dict.add_symbol('a_known_smb')

       # Example of use
       phrase = ['a', 'known', 'symbol', 'equal', 'to', 'another', 'one']
       fuzzy_matches = self.sym_dict.fuzzy_match_head(phrase)
       self.assert_equal(([(0.65079365079365081, 'a_known_smb')], ['equal', 'to', 'another', 'one']),
                         fuzzy_matches,
                         epsilon=0.005,
                         mess="Fuzzy match of phrase %s to a symbol failed." % phrase)
       
       # Testing under different conditions
 
###############################################################
# Assertions.
# 
# Use these methods to check the state of the class.
###############################################################

   def assert_head_matches_symbol(self, phrase, expected_matches, mess=""):
       got_matches = self.sym_dict.match_head(phrase)
       mess = mess + "\nSymbol matches for phrase %s were wrong" % repr(phrase)
       self.assert_equal(expected_matches, got_matches, mess)
       
   def assert__match_pseudo_symbol__returns(self, pseudo_symbol,
          expected_matches, use_match_threshold=None, mess=""):
       matches = self.sym_dict.match_pseudo_symbol(pseudo_symbol, 
                                                   use_match_threshold=use_match_threshold)
       good_matches = matches[0]
       weak_matches = matches[1]
       forbidden_matches = matches[2]
       self.assert_equal(expected_matches[0], good_matches, epsilon=0.01,
                         mess="List of good symbol matches were wrong for pseudo-symbol '%s'" % pseudo_symbol)
       self.assert_equal(expected_matches[1], weak_matches, epsilon=0.01,
                         mess="List of weak symbol matches were wrong for pseudo-symbol '%s'" % pseudo_symbol)
       self.assert_equal(expected_matches[2], forbidden_matches, epsilon=0.01,
                         mess="List of forbidden symbol matches were wrong for pseudo-symbol '%s'" % pseudo_symbol)
