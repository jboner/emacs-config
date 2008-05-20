##############################################################################
# VoiceCode, a programming-by-voice environment
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
# (C)2000, National Research Council of Canada
#
##############################################################################

import cPickle, exceptions
import Object

class PickledObject(Object.Object):
    """
    Class for an object that can pickle/unpickle itself to a file.
    
    **INSTANCE ATTRIBUTES**
    
    *STR pickle_fname=None* -- Name of the file to/from which the file is to be
    pickled/unpickled.

    CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, pickle_fname=None, **args_super):

        #
        # First, create a pickle_fname attribute, so that unpickle can access it
        #
        self.decl_attrs({'pickle_fname': pickle_fname})
        
        #
        # Then override values read from pickle file, with values received
        # as arguments
        #
        self.deep_construct(PickledObject, \
                            {'pickle_fname': pickle_fname}, \
                            args_super, \
                            {})

    def pickle(self, alt_file = None):
        """Saves the object to file.

        **INPUTS**

        *STR alt_file* -- name of file to which to save the object, or
        None to use the default of self.pickle_fname.  If alt_file is omitted 
        and self.pickle_fname is None, don't write to file.
        
        **OUTPUTS**
        
        *none* -- 
        """

#        print '-- PickledObject.pickle:  self.pickle_fname=%s' % self.pickle_fname
        
        if alt_file is None:
            pickle_file = self.pickle_fname
        else:
            pickle_file = alt_file
        if pickle_file != None:
            try:
                a_file = open(pickle_file, 'w')
                cPickle.dump(self, a_file)
                a_file.close()
            except exceptions.Exception, mess:
                print 'Error writing %s to file \'%s\'\n%s' \
                    % (self, pickle_file, mess)


    def unpickle(self, alt_file = None):
        """Reads the object from file.

        **INPUTS**
        
        *STR alt_file* -- name of file from which to restore the object, or
        None to use the default of self.pickle_fname.  If alt_file is omitted 
        and self.pickle_fname is None, leave the object unchanged
        

        **OUTPUTS**
        
        *none* -- 
        """

#        print '-- PickledObject.unpickle: self.pickle_fname=%s' % self.pickle_fname
        
        if alt_file is None:
            pickle_file = self.pickle_fname
        else:
            pickle_file = alt_file
        if pickle_file != None:
            try:
                orig_pickle_fname = self.pickle_fname
                a_file = open(pickle_file, 'r')
                tmp = cPickle.load(a_file)
                a_file.close()
                self.__dict__ = tmp.__dict__
                
                #
                # Override value of self.pickle_fname which was read from file
                #
                self.pickle_fname = orig_pickle_fname
            except exceptions.Exception, mess:
                print 'Error reading %s from file \'%s\'\n%s' \
                    % (self, pickle_file, mess)



