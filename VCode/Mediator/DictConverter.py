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
# (C)2003, David C. Fox
#
##############################################################################

"""abstract classes defining a system for converting dictionaries of
object attributes from one version to another.  Ultimately, these
dictionaries are meant to be passed to a method which initializes an
object from those attributes.
"""

from Object import Object, OwnerObject
import debug

class ConversionFailure(RuntimeError):
    """exception raised when DictConverter is unable to convert a
    dictionary to its final version
    """
    def __init__(self, reason):
        """
        **INPUTS**

        *STR reason* -- reason for failure
        """
        RuntimeError.__init__(self, reason)

class DictConverter(Object):
    """abstract class which converts a dictionary representing
    attributes of a given class from one or more versions to the current
    version of the class.
    """
    def __init__(self, **args):
        self.deep_construct(DictConverter, {}, args)

    def dict_class(self):
        """indicates the class corresponding to the dictionaries
        we are converting

        **INPUTS**

        *none*

        **OUTPUTS**

        *CLASS* -- the class corresponding to the dictionaries
        """
        debug.virtual('DictConverter.dict_class')

    def final_version(self):
        """the final version to which DictConverter converts
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- the final version to which DictConverter converts
        """
        debug.virtual('DictConverter.final_version')

    def known_versions(self):
        """returns a list of known initial versions from which the
        converter will convert

        **INPUTS**

        *none*

        **OUTPUTS**

        *[INT]* -- the initial versions which this DictConverter knows
        """
        debug.virtual('DictConverter.known_versions')

    def known_version(self, version):
        """is the specified version one from which the
        converter will convert

        **INPUTS**

        *INT version* -- initial version to check

        *none*

        **OUTPUTS**

        *BOOL* -- true if the converter can convert from this version to
        final_version
        """
        debug.virtual('DictConverter.known_version')

    def convert(self, original, initial_version):
        """converts a dictionary from one version to another

        NOTE: If DictConverter is unable to convert the dictionary, it
        will raise a ConversionFailure exception

        **INPUTS**

        *INT initial_version* -- initial version of the dictionary

        *[ANY:ANY] original* -- original dictionary
        
        *none*

        **OUTPUTS**

        *[ANY:ANY]* -- final dictionary
        """
        debug.virtual('DictConverter.convert')

class SingleVersionDictConverter(DictConverter):
    """abstract subclass of DictConverter which converts a dictionary 
    between one pair of versions
    """
    def __init__(self, **args):
        self.deep_construct(SingleVersionDictConverter, {}, args)

    def initial_version(self):
        """the initial version from which DictConverter converts
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- the initial version from which DictConverter converts
        """
        debug.virtual('DictConverter.initial_version')

    def known_versions(self):
        """returns a list of known initial versions from which the
        converter will convert

        **INPUTS**

        *none*

        **OUTPUTS**

        *[INT]* -- the initial versions which this DictConverter knows
        """
        return [self.initial_version()]

    def known_version(self, version):
        """is the specified version one from which the
        converter will convert

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the converter can convert from this version to
        final_version
        """
        return version == self.initial_version()

class BadConverter(RuntimeError):
    def __init__(self, message):
        RuntimeError.__init__(self, message)

class WrongClass(BadConverter):
    def __init__(self, wrong, right):
        message = "Converter's dictionary class %s\n" % str(wrong)\
                + "is not a subclass of %s" % str(right)
        BadConverter.__init__(self, message)

class UnknownFinalVersion(BadConverter):
    def __init__(self, version):
        message = "Converter's final version %s is unknown" % version
        BadConverter.__init__(self, message)


class CompoundDictConverter(Object):
    """concrete subclass of DictConverter containing a set of
    references to instances of a concrete subclass of 
    SingleVersionDictConverter 
    """
    def __init__(self, dict_class, final_version, **args):
        self.deep_construct(CompoundDictConverter, 
            {'target': final_version,
             'd_class': dict_class,
             'converters': {}
            },
            args)

    def dict_class(self):
        """indicates the class corresponding to the dictionaries
        we are converting

        **INPUTS**

        *none*

        **OUTPUTS**

        *CLASS* -- the class corresponding to the dictionaries
        """
        return self.d_class

    def final_version(self):
        """the final version to which DictConverter converts
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- the final version to which DictConverter converts
        """
        return self.target

    def known_versions(self):
        """returns a list of known initial versions from which the
        converter will convert

        **INPUTS**

        *none*

        **OUTPUTS**

        *[INT]* -- the initial versions which this DictConverter knows
        """
        return self.converters.keys()

    def known_version(self, version):
        """is the specified version one from which the
        converter will convert

        **INPUTS**

        *INT version* -- initial version to check

        *none*

        **OUTPUTS**

        *BOOL* -- true if the converter can convert from this version to
        final_version
        """
        return self.converters.has_key(version)

    def add_converter(self, converter):
        """Adds a new SingleVersionDictConverter instance to the
        compound converter

        NOTE: on failure, add_converter will raise various subclasses of
        BadConverter:
        
        If converter's dict_class is not a subclass of the compound 
        converter's dict_class, it will raise WrongClass.

        If converter.final_version is not equal to either the compound
        converters final_version or a known version, it will raise
        UnknownFinalVersion

        If converter.initial_version is already known, the converter
        will be ignored, but add_converter will return false

        **INPUTS**

        *SingleVersionDictConverter converter* -- the new converter

        **OUTPUTS**

        *BOOL* -- true if the single-version converter was successfully 
        added to the compound converter
        """
        dict_class = converter.dict_class()
        if not issubclass(dict_class, self.d_class):
            raise WrongClass(dict_class, self.d_class)
        version = converter.final_version()
        if version != self.final_version() \
            and not self.known_version(version):
            raise UnknownFinalVersion(version)
        initial = converter.initial_version()
        if self.known_version(initial):
            return 0
        self.converters[initial] = converter

    def convert(self, original, initial_version):
        """converts a dictionary from one version to another

        NOTE: If DictConverter is unable to convert the dictionary, it
        will raise a ConversionFailure exception

        **INPUTS**

        *INT initial_version* -- initial version of the dictionary

        *[ANY:ANY] original* -- original dictionary
        
        *none*

        **OUTPUTS**

        *[ANY:ANY]* -- final dictionary
        """
        d = original
        version = initial_version
        if not self.known_version(version):
            raise ConversionError('Unknown dictionary version %s' % version)
        while version != self.target:
            try:
                c = self.converters[initial_version]
            except KeyError:
                raise ConversionFailure("unknown version %s" % initial_version)
            d = c.convert(d, version)
            version = c.final_version()
        return d

