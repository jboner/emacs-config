import debug
import TestCaseWithHelpers
from SymbolResult import SymbolResult


class SymbolResultTest(TestCaseWithHelpers.TestCaseWithHelpers):
     
    def __init__(self, name):
        TestCaseWithHelpers.TestCaseWithHelpers.__init__(self, name)
        self.sym_res1 = None
    
    def setUp(self):
        self.written1 = 'some_symb'
        self.sym_res1 = SymbolResult(native_symbol = 'some_symb', 
                                     spoken_phrase = ['some', 'symbol'], 
                                     exact_matches = ['SomeSymb'],
                                     as_inserted='',
                                     buff_name=None,
                                     builder_preferences=['std_underscores', 'std_intercaps',
                                                          'std_all_caps_underscores'],
                                     possible_matches = [(2, 's_sym'), (1, 'sSmb'), (3, 'so_sbl')],
                                     forbidden=None,
                                     new_symbol=0,
                                     in_utter_interp=None)
                                                         
    def tearDown(self):
        pass
        
    def test_fixture_initialisation(self):
        pass
        
    def test_suggestions_list(self):
        self.assert_equal(
                     ['some_symb', 'SomeSymb', 'so_sbl', 's_sym', 'sSmb',
                     'some_symbol', 'SomeSymbol', 'SOME_SYMBOL'], 
                     self.sym_res1.suggestions_list(), 
                     "Suggested alternate forms for symbol were wrong.")