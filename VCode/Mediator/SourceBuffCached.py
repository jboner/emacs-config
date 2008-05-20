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
# (C)2001, National Research Council of Canada
#
##############################################################################

"""Use this interface if the link between VoiceCode and the external
buffer is slow"""


import debug
from debug import trace, tracing

import SourceBuff
import util

from Object import Object

class SourceBuffCached(SourceBuff.SourceBuffWithServices):
    
    """Interface optimised for editors that communicate with VoiceCode
    through a slow link.

    In order to minimise communication between VoiceCode and the
    editor, *SourceBuffCached* keeps a local copy of the state of the
    buffer. This avoids having to query the editor for the same
    information all the time.

    Methods in *SourceBuffCached* assume that:

    - The cached information will be synchronised with the editor's
      state at the beginning of every utterance.

    - *SourceBuffCached* methods that affect the state of the editor
      *directly* (that is, without going through an other [AppState]
      or [SourceBuff] method) will synchronize the cache before
      exiting.

    For any method *readingMethod* that reads the state of the
    external editor, *SourceBuffCached* should implement two methods:

    *readingMethod* -- This is the public method (usually a method of
     [SourceBuff]) used to read the state. It usually checks to see if
     the value is cached, and if so, it reads if from cache. If the
     value is not cached, it will retrieve it from the external
     application and cache it.
     
    *_readingMethod_from_app* -- This is a private method which reads
     the state directly from the external editor. Such methods are not
     expected to save the read state to cache (this will done by
     [synchronize_with_app] method).

    For any method *writingMethod* that changes the state of the
    external editor, *SourceBuffCached* assumes that subclasses will
    define *writingMethod* such that it invokes the change on the
    external editor AND make sure to synchronise the cache with the
    external editor.

    **INSTANCE ATTRIBUTES**

    {STR: STR} *cache* -- Key is the name of a cached information
    about the buffer, and value is the value of that information.
    
    BOOL *use_cache=1* -- If *false*, disable use of the cache. Use this ONLY 
    if you are debugging a problem and suspect it has something to do with the 
    caching.

    CLASS ATTRIBUTES**
    
    *none* -- 

    ..[synchronize_with_app] file:///./AppStateCached.AppStateCached.html#synchronize
    ..[AppState] AppState.AppState.html
    ..[SourceBuff] SourceBuff.SourceBuff.html"""

    def __init__(self, **attrs):
        self.init_attrs({'cache': {}})        
        self.deep_construct(SourceBuffCached,
                            {'use_cache': 1},
                            attrs
                            )
        # Set use_cache=0 ONLY if you are debugging a problem and suspect it 
        # has something to do with the caching.
#        self.use_cache = 0                    
                
        self.init_cache()


    def init_cache(self):
        """Initializes the cache from data acquired from external buffer.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """

#        print '-- SourceBuffCached.init_cache: called'

        self.cache = {'file_name': None,
                      'language_name': None, 'cur_pos': None,
                      'get_selection': None, 'get_text': None,
                      'get_visible': None, 'newline_conventions': None,
                      'pref_newline_convention': None}

    def _not_cached(self, name):
        if not self.use_cache:
           return 1
        else:
           return not (self.cache.has_key(name) and self.cache[name] != None)
           
    def _not_cached_multiple(self, names):
        trace('SourceBuffCached._not_cached_multiple', 'names=%s' % names)
        answer = 0
        for a_name in names:
           if self._not_cached(a_name):
              answer = 1
              break
        trace('SourceBuffCached._not_cached_multiple', 'returning %s' % answer)
        return answer  

    def _put_cache(self, name, value):
        if self.use_cache:
           self.cache[name] = value
           
    def _put_cache_multiple(self, names, values):
        if tracing('SourceBuffCached._put_cache_multiple'):
            trace('SourceBuffCached._put_cache_multiple', 'names=%s, values=%s' % (repr(names), repr(values)))
        for ii in range(len(names)):
            self._put_cache(names[ii], values[ii])
           
    def _get_cache(self, name):
        if self.use_cache:
           return self.cache[name]
        else:
           return None
           
    def _get_cache_multiple(self, names):
        trace('SourceBuffCached._get_cache_multiple', 'names=%s' % repr(names))
        values = []
        for a_name in names:
            values.append(self._get_cache(a_name))
        if tracing('SourceBuffCached._get_cache_multiple'):
            trace('SourceBuffCached._get_cache_multiple', 'returning values=%s' % repr(values))
        return values

    def _get_cache_element(self, elt_name, get_from_app_method):
        """Gets the value of an element that could be in cache.
        
        **INPUTS**
        
        STR *elt_name* -- name of the element in the cache.
        
        METHOD *get_from_app_method* -- the method to invoke in order to get the value of the 
        element directly from the client application instead of from the cache. 
        
        **OUTPUTS**
        
        ANY -- value of the element.
        """                 
        return self._get_cache_element_multiple([elt_name], get_from_app_method)[0]           


    def _get_cache_element_multiple(self, elt_names, get_from_app_method):
        """Gets the value of one or more elements that could be in cache.
        
        **INPUTS**
        
        [STR] *elt_names* -- list of names of the elements in the cache.
        
        METHOD *get_from_app_method* -- the method to invoke in order to get the value of the 
        elements directly from the client application instead of from the cache. We assume that
        the method returns the elements in the same order as they are listed in *elt_names*.
        
        **OUTPUTS**
        
        [ANY] *values* -- values of each of the elements.
        """ 
        trace('SourceBuffCached._get_cache_element_multiple', 'elt_names=%s, get_from_app_method=%s, self.use_cache=%s' % 
                                                              (repr(elt_names), get_from_app_method, self.use_cache))
        if not self.use_cache:
           debug.trace('SourceBuffCached._get_cache_element_multiple', 'not using cache')        
           values = apply(get_from_app_method)
           if len(elt_names) == 1:
               # if only one element, assume that get_from_app_method returns a single
               # value for that element, as opposed to a list of values for earch
               # element
               values = [values]           
        else:  
           debug.trace('SourceBuffCached._get_cache_element_multiple', 'looking up in cache')                   
           if self._not_cached_multiple(elt_names):
              debug.trace('SourceBuffCached._get_cache_element_multiple', 'cache is dirty... retrieving from app')                   
              values_from_app = apply(get_from_app_method)
              if len(elt_names) == 1:
                 # if only one element, assume that get_from_app_method returns a single
                 # value for that element, as opposed to a list of values for earch
                 # element
                 values_from_app = [values_from_app]
              self._put_cache_multiple(elt_names, values_from_app)
           else:
              debug.trace('SourceBuffCached._get_cache_element_multiple', 'cache element was up to date')
           values = self._get_cache_multiple(elt_names)
           
        if tracing('SourceBuffCached._get_cache_element_multiple'):
            debug.trace('SourceBuffCached._get_cache_element_multiple', 'returning values=%s' % repr(values))
        return values
        

    def file_name(self):
        """Returns the name of the file being displayed in this buffer.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        STR *name* -- 
        """
        return self._get_cache_element('file_name', self._file_name_from_app)
        

    def uncache_file_name(self):
        """uncaches the file name

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self._put_cache('file_name', None)

    def language_name(self):
        """Returns the name of the language a file is written in
        
        **INPUTS**        

        *none*

        **OUTPUTS**

        *STR* -- the name of the language
        """
        self._get_cache_element('language_name', self._language_name_from_app)

    def _language_name_from_app(self):
        """Returns the name of the language a file is written in
        
        **INPUTS**        

        *none*

        **OUTPUTS**

        *STR* -- the name of the language
        """
        debug.virtual('SourceBuffCached._language_name_from_app')

    def rename_buffer_cbk(self, new_buff_name):
        
        """AppState invokes this method when 
        AppState.rename_buffer_cbk is called to notify VoiceCode that 
        an existing text buffer has been renamed
        
        **INPUTS**

        STR *new_buff_name* -- new name of the buffer.
        
        **OUTPUTS**
        
        *none*
        
        ..[SourceBuff] file:///./SourceBuff.SourceBuff.html"""

        SourceBuff.SourceBuff.rename_buff_cbk(new_buff_name)
        self._put_cache('language_name', None)
        self._put_cache('file_name', None)

    def get_pos_selection(self):
        """retrieves current position of cursor and the range of 
        current selection
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS**
        
        *(INT, (INT, INT))* (pos, (start, end))
        
        pos is the offset into buffer of current cursor position
        start is the offset into the buffer of the start of the current
        selection.  end is the offset into the buffer of the character 
        following the selection (this matches Python's slice convention).
        """
        trace('SourceBuffCached.get_pos_selection', 'first, check cache...')
        values = self._get_cache_element_multiple(['cur_pos', 'get_selection'], 
                                                self._get_pos_selection_from_app)
        return values

    def _get_pos_selection_from_app(self):
        """retrieves current position of cursor and the range of 
        current selection directly from the external application
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS**
        
        *(INT, (INT, INT))* (pos, (start, end))
        
        pos is the offset into buffer of current cursor position
        start is the offset into the buffer of the start of the current
        selection.  end is the offset into the buffer of the character 
        following the selection (this matches Python's slice convention).
        """
        debug.virtual('SourceBuff._get_pos_selection_from_app')

    def get_text(self, start = None, end = None):
        """retrieves a portion of the buffer from the cache.

        **INPUTS**

        *INT start* is the start of the region returned.
        Defaults to start of buffer.

        *INT end* is the offset into the buffer of the character following 
        the region to be returned (this matches Python's slice convention).
        Defaults to end of buffer.

        **OUTPUTS**

        *STR* -- contents of specified range of the buffer
        """
        trace('SourceBuffCached.get_text', 'start=%s, end=%s' % (start, end))
                    
        text = self._get_cache_element('get_text', self._get_text_from_app)

        #
        # Note: cannot invoke self.make_valid_range() because it causes
        #       infinite recursion, ie:
        #       -> get_text() -> make_valid_range() -> len() -> get_text() -
        #      | _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ | 
        #             
        if start == None: start = 0
        if end == None: end = len(text)
        if end < start:
            tmp = end
            end = start
            start = tmp


        return text[start:end]


    def _get_text_from_app(self, start = None, end = None):
        """retrieves a portion of the buffer directly from external editor.

        **INPUTS**

        *INT start* is the start of the region returned.
        Defaults to start of buffer.

        *INT end* is the offset into the buffer of the character following 
        the region to be returned (this matches Python's slice convention).
        Defaults to end of buffer.

        **OUTPUTS**

        *STR* -- contents of specified range of the buffer
        """

        debug.virtual('SourceBuffCached._get_text_from_app')


    def get_visible(self):
        """Gets start and end positions of visible region from cache.

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* (start, end)
        """
        debug.trace('SourceBuffCached.get_visible', '** invoked')
        return self._get_cache_element('get_visible', self._get_visible_from_app)

    def _get_visible_from_app(self):
        
        """Gets start and end positions of visible region directly
        from external editor.

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* (start, end)
        """
        debug.virtual('SourceBuff._get_visible_from_app')        

    def len(self):
        """return length of buffer in characters from cache.

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* length 
        """
        return len(self.contents())


    def newline_conventions(self):
        
        """Returns a list of the forms of newline the editor can
        recognise for this buffer (read from cache).
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        return self._get_cache_element('newline_conventions', self._newline_conventions_from_app)

    def _newline_conventions_from_app(self):
        
        """Returns a list of the forms of newline the editor can
        recognise for this buffer (read directly from editor).
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        debug.virtual('SourceBuffCached._newline_conventions_from_app')

    def pref_newline_convention(self):
        
        """Returns the form of newline that the editor prefers for
        this buffer (read from cache).
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        return self._get_cache_element('pref_newline_convention', self._pref_newline_convention_from_app)

    def _pref_newline_convention_from_app(self):
        
        """Returns the form of newline that the editor prefers for
        this buffer (read directly from editor).
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        debug.virtual('SourceBuffCached._pref_newline_convention_from_app')


    #
    # Callback methods. These are invoked by the external editor to notify
    # VoiceCode that certain events have taken place in the editor.
    #
    def delete_cbk(self, range):
        
        """External editor invokes that callback to notify VoiceCode
        of a deletion event.
      
        **INPUTS**
        
        (INT, INT) *range* -- Start and end pos of range to be deleted
        

        **OUTPUTS**
        
        *none* -- 
        """

#        if range == None:
#            range = self.get_selection()
# bad: if this gets the selection from the application, it will be all
# screwed up because the application will already have made the change.
# Basically, callbacks should never use defaults for the range

        SourceBuff.SourceBuff.delete_cbk(self, range)

        if self._not_cached('get_text'):
# if we don't have the buffer contents cached, just get the entire
# current contents (which should already include the deletion), thereby
# caching it
#            self.get_text()
# 
# Oops - this causes major problems because there may already have been
# other changes to the buffer, whose change callbacks are still in the
# queue.  Therefore, the safe thing to do is to leave the buffer
# uncached until the next time we explicitly synchronize with the 
# application (which first flushes all updates from the listen_msgr)
            trace('SourceBuffCached.delete_cbk.short', 
                'no cache - ignoring callback')
            pass
        else:
            old_text = self.get_text()
            self._put_cache('get_text', old_text[:range[0]] + old_text[range[1]:])

        self.uncache_data_after_buffer_change(what_changed = 'get_text')
        

    def insert_cbk(self, range, text):
        """External editor invokes that callback to notify VoiceCode
        of an insertion event.

        **INPUTS**
        
        (INT, INT) *range* -- Start and end position of text to be
        replaced by the insertion. 

        STR *text* -- Text to be inserted

        **OUTPUTS**
        
        *none* -- 
        """
        if tracing('SourceBuffCached.insert_cbk.short'):
            trace('SourceBuffCached.insert_cbk.short', 
                'range=%s, len(text) = %d, text="%s..."' \
                % (range, len(text), text[0:60]))
        if tracing('SourceBuffCached.insert_cbk'):
            trace('SourceBuffCached.insert_cbk', 'range=%s, text=\'%s\'' % (range, text))

#        if range == None:
#            range = self.get_selection()
# bad: if this gets the selection from the application, it will be all
# screwed up because the application will already have made the change.
# Basically, callbacks should never use defaults for the range

        SourceBuff.SourceBuff.insert_cbk(self, range, text)
        if self._not_cached('get_text'):
# if we don't have the buffer contents cached, just get the entire
# current contents (which should already include the deletion), thereby
# caching it
#            self.get_text()
# 
# Oops - this causes major problems because there may already have been
# other changes to the buffer, whose change callbacks are still in the
# queue.  Therefore, the safe thing to do is to leave the buffer
# uncached until the next time we explicitly synchronize with the 
# application (which first flushes all updates from the listen_msgr)
            trace('SourceBuffCached.insert_cbk.short', 
                'no cache - ignoring callback')
            pass
        else:
            trace('SourceBuffCached.insert_cbk', 'updating cached value')
            old_text = self.get_text()
            self._put_cache('get_text', old_text[:range[0]] + text + \
                     old_text[range[1]:])

        self.uncache_data_after_buffer_change(what_changed = 'get_text')
        

    def contents_cbk(self, text):
        """External editor invokes that callback to inform VoiceCode
        of the complete contents of the buffer.
        
        **INPUTS**
        
        STR *text* -- Up-to-date contents of the buffer

        **OUTPUTS**
        
        *none* -- 
        """
        if tracing('SourceBuffCached.contents_cbk.short'):
            trace('SourceBuffCached.contents_cbk.short', 
                'len(text) = %d, text="%s..."' \
                % (len(text), text[0:60]))
        if tracing('SourceBuffCached.contents_cbk'):
            trace('SourceBuffCached.contents_cbk', 'range=%s, text=\'%s\'' % (range, text))


        SourceBuff.SourceBuff.contents_cbk(self, text)
        if self._not_cached('get_text'):
# if contents are not cached, cache them
            self._put_cache('get_text', text)
            self.uncache_data_after_buffer_change(what_changed = 'get_text')
            if tracing('SourceBuffCached.contents_cbk'):
                trace('SourceBuffCached.contents_cbk', 
                    ('** upon exit, self._get_cache("cur_pos")=%s,' +
                     ' self._get_cache("get_text")=%s') % \
                    (self._get_cache("cur_pos"), 
                    repr(self._get_cache("get_text"))))
        else:
# otherwise, treat this as an insert_cbk
            start, end, change = \
                find_difference.find_difference(self.cache['get_text'], text)
            self.insert_cbk(range = (start, end), text = change)
        

    def pos_selection_cbk(self, pos, selection, visible_range=None):
        """External editor invokes that callback to notify VoiceCode
        of a change in the current position or selection

        **INPUTS**
        
        INT *pos* -- Position the cursor was moved to.

        (INT, INT) *selection* -- Start and end position of selected text
        
        (INT, INT) *visible_range* -- Start and end position of the text 
        that is currently visible on the screen.

        
        **OUTPUTS**
        
        *none* -- 
        """
        trace('SourceBuffCached.pos_selection_cbk',
            'pos is %d, selection is %d, %d' % (pos, selection[0],
            selection[1]))
        self._put_cache('get_selection', selection)
        self._put_cache('cur_pos', pos)
        self._put_cache('get_visible', visible_range) 

    def visible_range_cbk(self, visible_range):
        """External editor invokes that callback to notify VoiceCode
        of a change in the range visible in current buffer

        **INPUTS**
        
        (INT, INT) *visible_range* -- New range of visible text in the buffer.
        
        **OUTPUTS**
        
        *none* -- 
        """
        pass


            
# DCF: this should only be called after changes to the buffer *contents*
#        self.uncache_data_after_buffer_change('get_selection')            

    def uncache_data_after_buffer_change(self, what_changed=None):
        trace('SourceBuffCached.uncache_data_after_buffer_change',
              'invoked, what_changed="%s"' % what_changed)
        #
        # Uncache data that may have become obsolete as a result of a
        # buffer change.
        #
        for cache_entry_name in ('cur_pos', 'get_visible', 'get_selection'):
            trace('SourceBuffCached.uncache_data_after_buffer_change',
                  '** cache_entry_name="%s"' % cache_entry_name)
            
            if cache_entry_name != what_changed:
                #
                # Don't uncache the data that was changed, because we assume
                # that it has been cached to the appropriate value.
                #
                self._put_cache(cache_entry_name, None)
                
        trace('SourceBuffCached.uncache_data_after_buffer_change',
              'exited')

