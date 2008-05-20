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

"""SourceBuff mixins that delegates a series of methods to supporting services."""


from Object import OwnerObject
import sb_services

        
class WithStateService(OwnerObject):
    """a SourceBuff mixin that uses an SB_ServiceState object to implement 
    methods to store/compare/restore buffer states"""
    def __init__(self, state_srv = None,
                      **attrs):
        self.deep_construct(WithStateService,
                            {'state_srv': state_srv},
                            attrs
                            )
        if self.state_srv is None:
            self.state_srv = sb_services.SB_ServiceFullState(buff = self)
        self.add_owned('state_srv')

    def _state_cookie_class(self):
        """returns the class object for the type of cookie used by
        store_current_state.

        **INPUTS**

        *none*

        **OUTPUTS**

        *CLASS* -- class of state cookies corresponding to this
        SourceBuff

        """
        return self.state_srv._state_cookie_class()
        
    def store_current_state(self):
        """stores the current state of the buffer, including both the
        contents and the current selection, for subsequent restoration.
        store_current_state returns a "cookie" which can be passed to
        restore_state or compare_with_current.  The type and attributes
        of the cookie will depend on the specific subclass of
        SourceBuff.  In the most straightforward implementation, it 
        may include a copy of the entire contents of the
        buffer and the selection.  In other cases, particularly when the
        editor or SourceBuff provides an internal undo stack, it may simply be a
        reference to a point in this stack.
        
        Important Notes:
        
        You should only pass the cookie to methods of
        the SAME SourceBuff object from which it came.  Generally,
        cookies can not be pickled and retrieved.

        The type of cookie will vary with the concrete subclass 
        of SourceBuff.  The corresponding class object is 
        returned by _state_cookie_class.  However, external callers
        should not depend on the type, attributes, or methods 
        of the cookie.

        **INPUTS**

        *none*

        **OUTPUTS**

        *SourceBuffState* -- state cookie (see above)
        """
        return self.state_srv.store_current_state()

    def get_state_pos_selection(self, cookie):
        """retrieves the position and selection from a given state
        cookie.  

        **INPUTS**

        *SourceBuffCookie cookie* -- see store_current_state.  Note that
        SourceBuffCookie is a dummy type, not an actual class.  The
        actual type will vary with SourceBuff subclass.

        **OUTPUTS**

        *(INT, (INT, INT))* -- position and selection at the time the
        cookie was created by store_current_state, or None if the cookie
        is invalid (usually because the state corresponding to the cookie 
        has been lost).
        """
        return self.state_srv.get_state_pos_selection(cookie)

    def compare_selection_with_current(self, cookie):
        """compares the current buffer position and selection to these
        values at the time when the cookie was returned by 
        store_current_state.  If the state corresponding to the cookie has
        been lost, compare_with_current will return false.

        **INPUTS**

        *SourceBuffCookie cookie* -- see store_current_state.  Note that
        SourceBuffCookie is a dummy type, not an actual class.  The
        actual type will vary with SourceBuff subclass.

        **OUTPUTS**

        *BOOL* -- true if position and selection are the same, false if 
        they are not, or if it cannot be determined due to 
        expiration of the cookie
        """
        return self.state_srv.compare_selection_with_current(cookie)

    def compare_state_selections(self, first_cookie, second_cookie):
        """compares the selection and cursor positions at the times when
        two cookies were returned by store_current_state.
        If the state corresponding to either cookie has
        been lost, compare_states will return false.

        This method does not synchronize with the editor prior to
        comparing with "current".  To ensure that the "current" state 
        is really current, the caller must synchronize.
        (This avoids having duplicate synchronize calls 
        when comparing with the current state of more than one buffer).

        **INPUTS**

        *SourceBuffCookie* first_cookie, second_cookie -- see 
        store_current_state.  Note that SourceBuffCookie is a dummy 
        type, not an actual class.  The actual type will vary with 
        SourceBuff subclass.

        **OUTPUTS**

        *BOOL* -- true if position and selection are the same, false if 
        they are not, or it cannot be determined due to expiration of 
        either cookie
        """
        return self.state_srv.compare_state_selections(first_cookie, 
            second_cookie)
 
    def restore_state(self, cookie):
        """restores the buffer to its state at the time when
        the cookie was returned by store_current_state.  Both the
        contents and the selection will be restored.  However, other
        data, such as the search history, may not.  The restore
        operation can fail, which will be indicated by a return value of
        0, so the caller should always check the return value.
        
        **INPUTS**

        *SourceBuffState cookie* -- see above.

        **OUTPUTS**

        *BOOL* -- true if restore was successful

        """
        return self.state_srv.restore_state(cookie)
      
    def compare_states(self, first_cookie, second_cookie, selection = 0):
        """compares the buffer states at the times when
        two cookies were returned by store_current_state.  By default,
        only the buffer contents are compared, not the selection, unless
        selection == 1.  If the state corresponding to either cookie has
        been lost, compare_states will return false.

        **INPUTS**

        *SourceBuffCookie* first_cookie, second_cookie -- see 
        store_current_state.  Note that SourceBuffCookie is a dummy 
        type, not an actual class.  The actual type will vary with 
        SourceBuff subclass.

        *BOOL* selection -- compare selection as well as contents

        **OUTPUTS**

        *BOOL* -- true if states are the same, false if they are not, or
        it cannot be determined due to expiration of either cookie
        """
        return self.state_srv.compare_states(first_cookie,
            second_cookie, selection)


    def compare_with_current(self, cookie, selection = 0):
        """compares the current buffer state to its state at the time when
        the cookie was returned by store_current_state.  By default,
        only the buffer contents are compared, not the selection, unless
        selection == 1.  If the state corresponding to the cookie has
        been lost, compare_with_current will return false.

        **INPUTS**

        *SourceBuffState cookie* -- see store_current_state.

        *BOOL* selection -- compare selection as well as contents

        **OUTPUTS**

        *BOOL* -- true if state is the same, false if it is not, or
        it cannot be determined due to expiration of the cookie
        """
        return self.state_srv.compare_with_current(cookie, selection)
        
    def valid_cookie(self, cookie):
        """checks whether a state cookie is valid or expired.
        If the state corresponding to the cookie has
        been lost, valid_cookie will return false.

        **INPUTS**

        *SourceBuffState cookie* -- see store_current_state. 

        **OUTPUTS**

        *BOOL* -- true if cookie is valid (i.e. restore_state should be
        able to work)
        """
        return self.state_srv.valid_cookie(cookie)
