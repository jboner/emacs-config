from wxPython.wx import *
import WinSystemMSW
import debug
import TestCaseWithHelpers
import MediatorConsoleWX
from SymbolResult import SymbolResult
import string, time
from SpokenUtterance import MockSpokenUtterance
from CmdInterp import MockUtteranceInterpretation

class DlgCanDisplayModallyWithoutBlockingMixin:
   """use this mixin when regression testing dialogs. It makes it 
   possible to display the dialog "modally" without actually blocking
   for user events.
   
   Use this mixin for DlgModelView. 
   
   Use ViewCanDisplayModallyWithoutBlockingMixin for ViewLayer."""

   def __init__(self, **args):
      self.deep_construct(ViewCanDisplayModallyWithoutBlockingMixin,
                          {},
                          args
                          )
                          
   def was_displayed_modally(self):
      return self.view().was_displayed_modally
      


class ViewCanDisplayModallyWithoutBlockingMixin:
   """use this mixin when regression testing dialogs. It makes it 
   possible to display the dialog "modally" without actually blocking
   for user events

   Use this mixin for ViewLayer.
   
   Use DlgCanDisplayModallyWithoutBlockingMixin for DlgModelView. 
   """

   def __init__(self, **args):
      self.deep_construct(ViewCanDisplayModallyWithoutBlockingMixin,
                          {'was_displayed_modally': None,
                           'being_displayed_modally': None},
                          args
                          )
                          
   def ShowModal(self):
      debug.trace('ViewCanDisplayModallyWithoutBlockingMixin.ShowModal', '** invoked, self=%s')
      self.was_displayed_modally = 1
      self.being_displayed_modally = 1
      
   def IsModal(self):
      return self.being_displayed_modally

   def EndModal(self, event_type):
      debug.trace('ViewCanDisplayModallyWithoutBlockingMixin.EndModal', '** invoked')
      self.being_displayed_modally = None
      


class ReformatRecentSymbols_NonBlocking(DlgCanDisplayModallyWithoutBlockingMixin,
                                        MediatorConsoleWX.ReformatRecentSymbols):
   """implementation of ReformatRecentSymbols that does 
   not block for user input when it is displayed "modally"
   """
   def __init__(self, **args):
      self.deep_construct(ReformatRecentSymbols_NonBlocking,
                          {},
                          args
                          )

   def make_view(self):
      return ReformatRecentSymbolsViewWX_NonBlocking(console = self.console, 
                                                     parent = self.parent,
                                                     symbols = self.symbols,
                                                     model = self,
                                                     gram_factory = self.gram_factory)   

class ReformatRecentSymbolsViewWX_NonBlocking(ViewCanDisplayModallyWithoutBlockingMixin,
                                              MediatorConsoleWX.ReformatRecentSymbolsViewWX):
   """implementation of ReformatRecentSymbolsViewWX that does 
   not block for user input when it is displayed "modally"
   """
   def __init__(self, **args):
      self.deep_construct(ReformatRecentSymbolsViewWX_NonBlocking,
                          {},
                          args
                          )
                          


class ReformatFromRecentWX_NonBlocking(DlgCanDisplayModallyWithoutBlockingMixin,
                               MediatorConsoleWX.ReformatFromRecentWX):
   """mock implementation of a dialog for reformatting a selected symbol.
   
   This mock implementation can be "displayed modally" without blocking.
   """
   
   def __init__(self, **args):
      debug.trace('ReformatFromRecentWX_NonBlocking.__init__', '** invoked')
      self.deep_construct(ReformatFromRecentWX_NonBlocking,
                          {},
                          args
                          )
      debug.trace('ReformatFromRecentWX_NonBlocking.__init__', '** upon exit, self.view()=%s, self.view_layer=%s' % (self.view(), self.view_layer))
                          
#>   def ShowModal(self):
#>      self.was_displayed_modally = 1
#>      self.being_displayed_modally = 1
#>      
#>   def IsModal(self):
#>      return self.being_displayed_modally
#>
#>   def EndModal(self):
#>      print '-- ReformatFromRecentWX_NonBlocking.EndModal: ** invoked'
#>      self.being_displayed_modally = None
#>            
   def make_view(self):
      return MockReformatFromRecentViewWX(console = self.console, parent = self.parent,
                                          model = self)   
#>   def reset(self, symbol):
#>      self.was_displayed_modally = None
#>      MediatorConsoleWX.ReformatFromRecentWX.reset(self, symbol)

class MockReformatFromRecentViewWX(ViewCanDisplayModallyWithoutBlockingMixin,
                                   MediatorConsoleWX.ReformatFromRecentViewWX):
   """mock implementation of the view layer for a dialog to reformat a
   seleted symbol"""
   
   def __init__(self, **args):
      self.deep_construct(MockReformatFromRecentViewWX, {}, args)
   
class MediatorConsoleWXTestCase(TestCaseWithHelpers.TestCaseWithHelpers): 
   """class for test cases that require a running mediator console ui."""
   def __init__(self, name):
      TestCaseWithHelpers.TestCaseWithHelpers.__init__(self, name)
      frame = wxFrame(None, wxNewId(), "test console", 
            wxDefaultPosition, wxDefaultSize)
 
      self.console = MediatorConsoleWX.MediatorConsoleWX(frame, win_sys = WinSystemMSW.WinSystemMSW())
      

class ReformatRecentTestCase(MediatorConsoleWXTestCase):    
     
    utter1 = MockSpokenUtterance(['new', 'symbol', 'one', 'one', 'equals', 'new', 'symbol', 'one', 'two'])
            
                                  
    sym1_1 = SymbolResult(native_symbol = 'new_symbol_1_1', 
                          spoken_phrase = ['new', 'symbol', 'one', 'one'], 
                          exact_matches = ['NewSymb1_1'],
                          as_inserted='',
                          buff_name=None,
                          builder_preferences=['std_underscores', 'std_intercaps',
                                               'std_all_caps_underscores'],
                          possible_matches = [(2, 'ns11'), (1, 'news1_1')],
                          forbidden=None,
                          new_symbol=0,
                          in_utter_interp=None)
                     
    
                         
                          
    sym1_2 = SymbolResult(native_symbol = 'new_symbol_1_2', 
                          spoken_phrase = ['new', 'symbol', 'one', 'two'], 
                          exact_matches = ['NewSymb1_2'],
                          as_inserted='',
                          buff_name=None,
                          builder_preferences=['std_underscores', 'std_intercaps',
                                               'std_all_caps_underscores'],
                          possible_matches = [(2, 'ns12'), (1, 'news1_2')],
                          forbidden=None,
                          new_symbol=0,
                          in_utter_interp=None)
    
    sym_list = [sym1_1, sym1_2]
                                   
    phrase1 = MockUtteranceInterpretation(utter1, symbols = sym_list, )
    sym1_1.in_utter_interp = phrase1
    sym1_2.in_utter_interp = phrase1

    
    def __init__(self, name):
        MediatorConsoleWXTestCase.__init__(self, name)
        self.dlg = None
    
    def setUp(self):
        self.mock_reformat_from_recent = \
           ReformatFromRecentWX_NonBlocking(console = self.console, 
                                     parent = None, symbol = ReformatRecentTestCase.sym1_1)
                 
        self.dlg = \
           ReformatRecentSymbols_NonBlocking(console = self.console, 
                                             parent = None, 
                                             symbols = ReformatRecentTestCase.sym_list, 
                                             gram_factory = None,
                                             dlg_reformat_from_recent = self.mock_reformat_from_recent)

        self.dlg.ShowModal()
        
                                                         
    def tearDown(self):
        self.console.destroy_main_frame()
        self.mock_reformat_from_recent.Destroy()
        self.dlg.Destroy()

    def assert_reformat_from_recent_invoked_with_symbol(self, symbol):
       self.assert_(self.dlg.dlg_reformat_from_recent.was_displayed_modally(),
                    "Reformat from recent dialog was not displayed")
       debug.trace('assert_reformat_from_recent_invoked_with_symbol',
                   "** symbol=%s, self.dlg.dlg_reformat_from_recent.symbol=%s" % (symbol, self.dlg.dlg_reformat_from_recent.symbol))
       self.assert_symbols_are_same(
                           symbol, self.dlg.dlg_reformat_from_recent.symbol,
                          "Reformat from recent dialog invoked with wrong symbol")

    def assert_symbols_are_same(self, expected, got, mess):
       self.assert_equal(expected.native_symbol, got.native_symbol, 
                         "\nnative_symbol attribute differed")
       self.assert_equal(expected.spoken_phrase, got.spoken_phrase, 
                         "\nspoken_phrase attribute differed")
       self.assert_equal(expected.spoken_phrase, got.spoken_phrase, 
                         "\nspoken_phrase attribute differed")

    def assert_symbols_were_reformatted_to(self, expected_reformatting_indices, mess=''):
       expected_reformattings = []
       for expected_indices in expected_reformatting_indices:
          this_symbol = self.dlg.symbols[expected_indices[0]]
          this_old_form = this_symbol.native_symbol()
          this_new_form = this_symbol.reformatted_to
          this_reformatting = (this_old_form, this_new_form)
          expected_reformattings.append(this_reformatting)
       expected_reformattings.sort()

       got_reformattings = []       
       for a_symbol in self.dlg.user_reformatted_symbols():
          got_reformattings.append((a_symbol.native_symbol(), a_symbol.reformatted_to))
       got_reformattings.sort()
       
       self.assert_equal(expected_reformattings, got_reformattings,
                                            mess + "\nList of reformattings was wrong.")
        
    def ___test_fixture_initialisation(self):
        self.assert_(self.dlg != None, "Symbool reformatting model not initialised properly.")
        
    def ___test_displayed_symbols(self):
        self.assert_sequences_have_same_content\
               ([
                     ['2', 'new_symbol_1_1', 'new symbol one one', 
                      'new symbol one one equals new symbol one two'], 
                     ['1', 'new_symbol_1_2', 'new symbol one two', 
                      'new symbol one one equals new symbol one two']
                 ], 
                self.dlg.displayed_symbols(),
               "Displayed utterances were wrong.")

    def test_choose(self):
       self.dlg.do_choose(0)
       self.assert_equal(0, self.dlg.selected_symbol_index(),
                          "Selected symbol was wrong.")
       self.assert_reformat_from_recent_invoked_with_symbol(self.dlg.symbols[0])
       
    def reformat_a_symbol(self, nth_symbol, iith_form_for_that_symbol, how='choose'):
       self.dlg.do_choose(nth_symbol)
       if how == 'choose':
          self.dlg.dlg_reformat_from_recent.do_choose_nth_form(iith_form_for_that_symbol)
       elif how == 'select_then_cancel':
          self.dlg.dlg_reformat_from_recent.do_select_nth_form(iith_form_for_that_symbol)
          self.dlg.dlg_reformat_from_recent.do_cancel()
       elif how == 'select_then_ok':
          self.dlg.dlg_reformat_from_recent.do_select_nth_form(iith_form_for_that_symbol)
          self.dlg.dlg_reformat_from_recent.do_ok()
       else:
          raise RuntimeError("unknown value for how=%s" % how)
       
       
    def ___test_do_ok(self):
       self.reformat_a_symbol(1, 1)
       self.reformat_a_symbol(0, 1)
       self.dlg.do_ok()
       self.assert_symbols_were_reformatted_to([(0, 1), (1, 1)])
       
    def ___test_do_cancel(self):
       self.reformat_a_symbol(1, 1)
       self.reformat_a_symbol(0, 1)
       self.dlg.do_cancel()
       self.assert_symbols_were_reformatted_to([])
       
    def ___test_do_cancel_on_reformat_from_recent_subdialog(self):
       self.reformat_a_symbol(1, 1, how='select_then_cancel')
       self.assert_symbols_were_reformatted_to([])
              
class ReformatFromRecentTestCase(MediatorConsoleWXTestCase):
    def __init__(self, name):
       MediatorConsoleWXTestCase.__init__(self, name)
       self.dlg = None
       
    def setUp(self):
       self.utter1 = MockSpokenUtterance(['new', 'symbol', 'one', 'one', 'equals', 'new', 'symbol', 'one', 'two'])                                  
       self.sym1_1 = SymbolResult(native_symbol = 'new_symbol_1_1', 
                                  spoken_phrase = ['new', 'symbol', 'one', 'one'], 
                                  exact_matches = ['NewSymb1_1'],
                                  as_inserted='',
                                  buff_name=None,
                                  builder_preferences=['std_underscores', 'std_intercaps',
                                                       'std_all_caps_underscores'],
                                  possible_matches = [(2, 'ns11'), (1, 'news1_1')],
                                  forbidden=None,
                                  new_symbol=0,
                                  in_utter_interp=None)

       self.dlg = ReformatFromRecentWX_NonBlocking \
                        (console = self.console, 
                        parent = None, symbol = self.sym1_1)
       self.dlg.reset(self.sym1_1)
       self.dlg.ShowModal()


    def tearDown(self):
        self.console.destroy_main_frame()
        self.dlg.Destroy()
        
    def assert_displayed_spoken_form_is(self, expected, mess=''):
        self.assert_string_contains(expected, 
                           self.dlg.view().intro(), 
                           "Spoken form displayed for the symbol was wrong.")

    def assert_displayed_form_is(self, expected, mess=''):
        self.assert_equal(expected, self.dlg.chosen_form(),
                           mess + "Corrected form displayed by view was wrong")
                           
    def assert_displayed_alternate_forms_are(self, expected, mess=''):
        self.assert_equal(expected, self.dlg.displayed_list_of_alternate_forms(),
                                                "Displayed utterances were wrong.")

    def assert_symbol_was_not_reformatted(self):
        self.assert_(not self.dlg.symbol.reformatted_to, "Symbol reformatted prematurely, or its reformatting was not undone as it should have")

    def assert_symbol_was_reformatted_to(self, expected_form):
        self.assert_equal(expected_form, self.dlg.symbol.reformatted_to,
                           "Symbol reformatted to the wrong form.")


    def assert_dialog_was_not_okayed(self, mess=''):
        self.assert_(not self.dlg.was_okayed, 
                     mess + "\nDialog was okayed when it should NOT have.")    

    def assert_dialog_was_okayed(self, mess=''):
        self.assert_(self.dlg.was_okayed, 
                     mess + "\nDialog was NOT okayed when it should have.")    

    def ___test_fixture_initialisation(self):
        self.assert_(self.dlg != None, "Reformat from recent dialog not initialised properly.")
        self.assert_displayed_form_is('new_symbol_1_1')
        self.assert_displayed_alternate_forms_are(self.sym1_1.suggestions_list())
        self.assert_displayed_spoken_form_is(string.join(self.sym1_1.spoken_phrase()))
        
    def ___test_on_select_form(self):
        self.dlg.do_select_nth_form(2)
        self.assert_displayed_form_is(self.sym1_1.suggestions_list()[2], 'Selecting new format did not change the displayed form.')
        self.assert_symbol_was_not_reformatted()

    def ___test_on_choose_form(self):
        self.dlg.do_choose_nth_form(2)
        self.assert_displayed_form_is(self.sym1_1.suggestions_list()[2], 'Selecting new format did not change the displayed form.')
        self.assert_symbol_was_reformatted_to(self.sym1_1.suggestions_list()[2])
        self.assert_dialog_was_okayed()
        
        
    def ___test_cancel(self):
        self.dlg.do_choose_nth_form(2)
        self.dlg.do_cancel()
        self.assert_symbol_was_not_reformatted()
        self.assert_dialog_was_not_okayed()
        
    def ___test_type_form(self):
        typed_form = '__new_symbol_1_1'
        self.dlg.do_type_form(typed_form)
        self.dlg.do_ok()
        self.assert_symbol_was_reformatted_to(typed_form)
        self.assert_dialog_was_okayed()
 
    # For now, can't implement this test because no way of 
    # programmatically sending a character to the wxListCtrl
    def ___________test_move_around_suggestions_list_with_arrow_keys(self):
        pass
        self.dlg.do_move_down_alternate_forms_with_arrow_keys(2)
        self.assert_assert_displayed_form_is('')