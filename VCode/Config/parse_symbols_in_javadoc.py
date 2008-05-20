import sys
import os
import re

# ----------------------------------------------------------------------
# Parses JavaDoc HTML files and compiles a file with all symbols found 
# (classes, interfaces and methods - skipping public constants for now.)
# ----------------------------------------------------------------------
class SymbolParser:

    def parse(self, startDir, symbolFileName): 
        files = parser.getRecursiveFileList(startDir, ['html'])
        symbolAcc = {}
        for file in files:
             parser.parseHTML(file, symbolAcc)
        parser.writeToFile(symbolFileName, symbolAcc.keys())

    def parseHTML(self, inputName, symbolAcc):
        print "Parsing " + inputName
        input = open(inputName, 'r')
        for line in input:
            if line[:10] == '<META NAME':
                symbol = line.split("\"")[3]
                length = len(symbol)
                if symbol[length - 5: length] == 'class':
                    symbol = symbol[:length - 5]
                    lst = symbol.split(".")
                    symbol = lst[len(lst) - 1]
                elif symbol[length - 9: length] == 'interface':
                    symbol = symbol[:length - 9]
                    lst = symbol.split(".")
                    symbol = lst[len(lst) - 1]
                elif symbol[length - 7: length] == 'package':
                    symbol = symbol[:length - 7]
                    lst = symbol.split(".")
                    symbol = lst[len(lst) - 1]
                elif symbol[length - 2: length] == '()':
                    symbol = symbol[:len(symbol) - 2] # remove () from methods

                # filter out all java constants, e.g all uppercase letters
                if len(re.findall("[A-Z0-9_]", symbol)) == len(symbol):
                    continue

                print "\t" + symbol
                symbolAcc[symbol.strip()] = ''
        input.close()

    def writeToFile(self, outputName, symbols):
        output = open(outputName, 'w')
        symbols.sort()
        for symbol in symbols: 
            output.write(symbol + '\n')
        output.close()

    def getRecursiveDirList(self, basedir):
        basedir = self.addSlash(basedir)
        subdirlist = []
        dirlist = []
        dirlist.append(basedir)

        try:
            for item in os.listdir(basedir):
                if os.path.isdir(os.path.join(basedir, item)):
                    dirlist.append(os.path.join(basedir, item))
                    subdirlist.append(os.path.join(basedir, item))
        except WindowsError:
            print "Error: you may not have permission to access all files and folders in the specified path."

        for subdir in subdirlist:
            dirlist += self.getRecursiveDirList(subdir)

        return dirlist

    def getRecursiveFileList(self, basedir, extensions=[]):
        basedir = self.addSlash(basedir)        
        subdirlist = []
        filelist = []

        try:
            if len(extensions) > 0:
                for item in os.listdir(basedir):
                    if os.path.isfile(os.path.join(basedir, item)):
                        if extensions.count(item[item.rfind('.') + 1:]) > 0:
                            filelist.append(os.path.join(basedir, item))
                    else:
                        subdirlist.append(os.path.join(basedir, item))
            else:
                for item in os.listdir(basedir):
                    if os.path.isfile(os.path.join(basedir, item)):
                        filelist.append(os.path.join(basedir, item))
                    else:
                        subdirlist.append(os.path.join(basedir, item))
        except WindowsError:
            print "Error: you may not have permission to access all files and folders in the specified path."
        except:
            print "Unknown Error"

        for subdir in subdirlist:
            filelist += self.getRecursiveFileList(subdir, extensions)

        return filelist

    def addSlash(self, dir):
        if dir[-1:] != '/':
            dir += '/'
        return dir

# expects the path to the JavaDoc API folder as input
for startDir in sys.argv[1:]:
    parser = SymbolParser()
    parser.parse(startDir, "java_std_lib.sym")


