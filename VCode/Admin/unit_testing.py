from vc_globals import unit_tests_dir, benchmark_dir
import exceptions, glob, os, re, string, sys
from debug import trace, config_traces, trace_fct, to_be_traced, \
                  activate_trace_id_substrings
from unittest import makeSuite, TestCase, TestSuite, TextTestRunner
import debug


def containsString(str, substring):
    return string.find(str, substring) >= 0

class VCTestSuite(TestSuite):
    def __init__(self, name_indexed_tests):
        self.invert_test_indexing(name_indexed_tests)
        TestSuite.__init__(self, self.test_name.keys())

    def invert_test_indexing(self, name_indexed_tests):
        self.test_name = {}
        for (a_test_name, a_test) in name_indexed_tests.items():
            self.test_name[a_test] = a_test_name

    def __call__(self, result):
        tests_so_far = 0
        total_tests = self.countTestCases()
        for test in self._tests:
            tests_so_far = tests_so_far + test.countTestCases()
            sys.stderr.write('\n... running %s (%s of %s)\n' %
                             (self.test_name[test], tests_so_far,
                              total_tests))
            if result.shouldStop:
                break
            test(result)
        return result
    

class VCPyUnitTestRunner(TextTestRunner):
    def __init__(self):
        self.all_suites = {}
        TextTestRunner.__init__(self)

    def get_test_class_name(self, test_class):
        a_match = re.match('\s*<\s*class\s+[\s\S]*?\\.([^\\.]*)\s*\'*>',
                           repr(test_class))
        return a_match.group(1)

    def add_suite(self, test_class, test_methods_prefix='test'):
        self.all_suites[self.get_test_class_name(test_class)] = makeSuite(test_class, test_methods_prefix)

    
    def generate_suite_to_run(self, test_names_list):
        test_suites_to_run = {}
        for a_suite_name in test_names_list:
            if a_suite_name == 'all':
                test_suites_to_run = self.all_suites
                break
            test_suites_to_run[a_suite_name] = self.all_suites[a_suite_name]
            
        suite_with_all_tests = VCTestSuite(test_suites_to_run)
            
        return suite_with_all_tests


    def run_tests_with_names(self, test_names_list):
        test_runner = TextTestRunner()        
        test_suite_to_run = self.generate_suite_to_run(test_names_list)
        self.run(test_suite_to_run)

class TestCaseDiffingOutputs(TestCase):
    
    """This class, captures STDOUT to a string and compares it to a
    benchmark file.

    The test fails if the two files differ.

    NOTE: All *test* methods of this class should:
    - invoke self.start_new_test() at the beginning
    - invoke self.compare_outputs() at the end

    There must be a way to automate this, but I fought with pyUnit for a while
    and couldn't figure it out.
    
    **INSTANCE ATTRIBUTES**
    
    [StreamMock] *output_captures* -- The [StreamMock] used to capture STDOUT

    FILE *old_stdout* -- What *sys.stdout* was set at before we started the
    output capture.
    
    CLASS ATTRIBUTES**
    
    *none* -- 
    """

    def start_new_test(self, test_name=None):
        self.output_capture = StreamMock('w')
        sys.stdout = self.output_capture
        self.test_name = test_name

    def file_names_root(self):
        class_name = "%s" % self.__class__
        a_match = re.match('[\s\S]*?.{0,1}([^.]*)$', class_name)
        class_name_root = a_match.group(1)
        return os.path.join(benchmark_dir, class_name_root)

    def file_names_root(self):
        class_name = "%s" % self.__class__
        a_match = re.match('[\s\S]*?.{0,1}([^.]*)$', class_name)
        test_file_name = a_match.group(1)
        if self.test_name:
            test_file_name = "%s-%s" % (test_file_name, self.test_name)
        return os.path.join(benchmark_dir, test_file_name)

    def benchmark_file_path(self):
        return self.file_names_root() + '.benchmark'
    
    def output_file_path(self):
        return self.file_names_root() + '.out'

    def compare_outputs(self):
        sys.stderr.write('STDERR -- TestCaseDiffingOutputs.compare_outputs: invoked\n')                
        benchmark_file = open(self.benchmark_file_path(), 'r')
        benchmark = benchmark_file.read()
        benchmark_file.close()

        sys.stderr.write('STDERR -- TestCaseDiffingOutputs.test: self.output_capture.stream=\'%s\'\n' % self.output_capture.stream) 
        sys.stderr.write('STDERR -- TestCaseDiffingOutputs.test: benchmark=\'%s\'\n' % benchmark)       
        

        if self.output_capture.stream != benchmark:
            output_file = open(self.output_file_path(), 'w')
            output_file.write(self.output_capture.stream)
            output_file.close()
            
        assert self.output_capture.stream == benchmark, \
               "Output file differs from benchmark.\nOutput file='%s'\nBenchmark file = '%s'" % (self.output_file_path(), self.benchmark_file_path())


    def setUp(self):
       self.old_stdout = sys.stdout
       self.start_new_test()

    def tearDown(self):
        sys.stderr.write('STDERR -- TestCaseDiffingOutputs.tearDown: self.output_capture.stream=\'%s\'\n' % self.output_capture.stream)                
        sys.stderr.write('STDERR -- TestCaseDiffingOutputs.tearDown: restoring stdout\n')        
        sys.stdout = self.old_stdout
        pass

    def runTest(self):
        self.test()
                   
test_runner = VCPyUnitTestRunner()

def define_all_test_suites():
    #
    # find all the .py files in %VCODE_HOME%/Admin/UnitTests/
    # and import them.
    #
    
    unit_test_files = glob.glob(os.path.join(unit_tests_dir, '*.py'))
    for a_test_file in unit_test_files:
        execfile(a_test_file)


class TraceTest(TestCase):
    def setUp(self):
        self.old_trace_fct = trace_fct
        self.old_to_be_traced = to_be_traced
        self.old_activate_trace_id_substrings = activate_trace_id_substrings

    def reset_traces_config(self):
        trace_fct = self.old_trace_fct
        to_be_traced = self.old_to_be_traced        
    
    def do_some_traces(self, status, activate_all=0, allow_trace_id_substring=None):
        global mock_stdout
        mock_stdout = StreamMock(mode='w')
        if activate_all:
            active_traces = 'all'
        else:
            active_traces = {'always_in_active_list': 1}
            
        config_traces(status=status,
                      active_traces=active_traces,
                      allow_trace_id_substrings=allow_trace_id_substring)
        trace('always_in_active_list', '')
        trace('never_in_active_list', '')
        trace('XXXXXalways_in_active_list', '')        
        self.reset_traces_config()        
        return mock_stdout.stream

    def what_was_printed(self):
        return '\nTraces printed: "%s"' % mock_stdout.stream

    def test_traces_not_printed_when_status_off(self):
        assert self.do_some_traces(status='off') == '', \
               'traces printed even though trace status was \'off\'' + \
               self.what_was_printed()                

    def test_traces_printed_when_active(self):
        assert containsString(self.do_some_traces('on'),
                                  '-- always_in_active_list'), \
               'active trace not printed even though trace status was \'on\'' + \
               self.what_was_printed()                

    def test_traces_not_printed_when_inactive(self):
        assert not containsString(self.do_some_traces('on'),
                              '-- never_in_active_list'), \
               'inactive trace was printed' + \
               self.what_was_printed()        

    def test_traces_printed_when_all_active(self):
        assert containsString(self.do_some_traces('on', activate_all=1),
                              '-- never_in_active_list'), \
               'inactive trace not printed even though overode all traces with active.' + \
               self.what_was_printed()
        
        assert containsString(self.do_some_traces('on', activate_all=1),
                              '-- always_in_active_list'), \
               'active trace not printed when all traces overriden with active.' + \
               self.what_was_printed()

    def test_substring_traces_not_printed_when_substrings_not_allowed(self):
        assert not containsString(self.do_some_traces('on'),
                                 'XXXalways_in_active_list'), \
               'substring trace printed eventhough substrings were not allowed' + \
               self.what_was_printed()

    def test_traces_printed_when_activated_through_substring(self):
        assert containsString(self.do_some_traces('on', allow_trace_id_substring=1),
                              'XXXXalways_in_active_list'), \
               'trace NOT printed eventhough activated through regexp' + \
               self.what_was_printed()
            
    def test_traces_not_printed_when_not_activated_through_substring(self):
        assert not containsString(self.do_some_traces('on', allow_trace_id_substring=1),
                                  '-- never_in_active_list'), \
               'trace printed eventhough NOT activated through regexp' + \
               self.what_was_printed()
                                      
test_runner.add_suite(TraceTest)


def run_all_pyunit_tests():
    test_runner.run_tests_with_names(['all'])


###############################################################################
# Some useful support classes for building tests
###############################################################################

class StreamMock:
    """Mimicks the behaviour of a file, except that it reads writes to a
    string.

    I bet there is something like this in Python, but I didn't find it."""
    
    def __init__(self, mode='r'):
        if re.search('r', mode):
            self.mode = 'r'
        else:
            self.mode = 'w'
            if not re.search('a', mode):
                self.stream = ''

        
    def write(self, str):
        self.stream = self.stream + str

mock_stdout = StreamMock()

define_all_test_suites()

if __name__ == '__main__':
    run_all_pyunit_tests()

