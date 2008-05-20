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
# (C)2002, National Research Council of Canada
#
##############################################################################

import sr_interface, util
import natlink


def help():
   print """

Usage: python toggle_nat_text.py user_name status
   
Turns NaturalText on (status=1) or off (status=0)."""

if __name__ == '__main__':
   opts, args = util.gopt(())
   if len(args) == 0:
      help()
   else:
      sr_interface.connect(args[0])
      natlink.execScript('SetNaturalText %s' % args[1])
      sr_interface.disconnect()
   
   
   