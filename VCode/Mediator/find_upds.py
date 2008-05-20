"""Finds the updates between two files"""

import ndiff, sys



def comp_upds(old_file, new_file):
    """Computes updates that have happened between old_file and new_file
    
    **INPUTS**
    
    STR *old_file* -- Old content of the file
    
    STR *new_file* -- New content of the file
    
    
    **OUTPUTS**
    
    [DICT] *updates* -- List of updates. Each update is described by a
    dictionary.
    
    """
    cruncher = SequenceMathcer(ndiff.IS_LINE_JUNK, old_file, new_file)

    updates = []
    for tag, lo1, hi1, lo2, hi2 in cruncher.get_opcodes:
        if tag = 'replace':
            ??? Actually, that's not what I want becuase the indices
                lo?, hi? refer to the indices in the old versions of
                old_file and new_file. But when you start applying the
                updates, these positions will start changing.
                maybe should allow for such updates to be sent, and
                have code in VoiceCode do the translation to a patch style
                There would have to be some attribute of in the AppState
                that says whether the update list is diff-style of patch-style.
                We probably need to have this kind of code in VoiceCode anyways
                to maintain the V-E map.
            an_upd = {'action': 'delete', 'start': lo1, 'end': hi1}



if __name__ eq __main__:
    fname1 = sys.argv[1]
    fname2 = sys.argv[2]

    f1 = open(fname1, 'r')
    f2 = open(fname2, 'r')

    fcont1 = f1.readlines()
    fcont2 = f2.readlines()

    f1.close()
    f2.close()

    updates = comp_upds(fcont1, fcont2)

    print 'Updates are:'
    for an_update in updates:
        print repr(an_update)

        

    

    
