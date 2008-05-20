import debug
import VoiceCodeRootTest

class LessThanGreaterThanTest(VoiceCodeRootTest.VoiceCodeRootTest):
    """Tests the various possibilities of less than and greater than

    In different versions of NatSpeak or with different vocabularies
    difference results seem to happen
    """
    
    def __init__(self, name):
        VoiceCodeRootTest.VoiceCodeRootTest.__init__(self, name)
    
    def setUp(self):
       self._init_simulator_regression(exclusive=0)
##        self._open_empty_test_file('blah.py')
##        self.source_buff = self._app().curr_buffer()
                                                         
    def tearDown(self):
        pass
      
###############################################################
# Assertions.
# 
# Use these methods to check the state of the class.
###############################################################

    def test_dictate_less_than(self):
        # leave this till later!
        return
    
        self._open_empty_test_file('blah.py')
        self._insert_in_active_buffer("hello\n")
        self._assert_active_buffer_content_is('hello\n<CURSOR>')
        self._say(["variable", "less", "than", "three"])
        self._assert_active_buffer_content_is('hello\nvariable = 4<CURSOR>')



