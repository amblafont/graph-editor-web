#print(" Pour que ça marche il faut firefox et ajouter geckodriver soit dans PATH")
print("This python3 script requires the selenium geckodriver (https://selenium-python.readthedocs.io/installation.html#drivers)")
print("Other requirements: the in_place package")
print()



from curses import KEY_COPY
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException
from selenium.common.exceptions import ElementNotInteractableException
from selenium.common.exceptions import UnexpectedAlertPresentException
from selenium.webdriver.common.desired_capabilities import DesiredCapabilities
from selenium.webdriver.support.ui import Select
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import WebDriverWait
from os.path import exists
from collections import namedtuple
import os
import pyperclip

# Diag = namedtuple('Diag' , 'content isFile isEdit isNew isLatex isGenerate isClipboard isMake')
Diag = namedtuple('Diag' , 'content isFile command')
Command = namedtuple('Command' , 'content prefix command')

import sys
import re
import in_place
import argparse
from enum import Enum, auto

MAGIC_STRING = "YADE DIAGRAM"
URL_YADE = os.getenv('YADE_URL')
if not URL_YADE:
    URL_YADE = 'https://amblafont.github.io/graph-editor/index.html'

DESCRIPTION = """
This script looks for lines ending with YADE DIAGRAM +command [diagram]")

- [diagram] (optional) is a filename, or a json encoded diagram (pasted from the editor by 
  using C-c  on some selected diagram, or from the content of a saved file).
- "command" is a character with specific meaning
    GENERAL COMMANDS
    * n : new diagram (which is edited)
    * e : edit diagram
          The edited diagram is saved either as a json encoded diagram or in the specified filename
          Latex generation is available after editing
    * m : latex generation inserted after the matching line, and make
    * c : latex copied to the clipboard    
"""

ID_SAVE_BUTTON = 'save-button'
ID_LOAD_BUTTON = 'load-button'



class Cmd(Enum):
    NEW = auto()
    EDIT = auto()
    MAKE = auto()
    COPY = auto()
    def isLatex(self):
        return self == self.MAKE or self == self.COPY
    def isInteractive(self):
        return self == self.NEW or self == self.EDIT

EXIT_KEY = 'e'
MAKE_KEY = 'm'
COPY_KEY = 'c'

CMD_DICT = { 'c': Cmd.COPY, 'e': Cmd.EDIT, 'm': Cmd.MAKE, 'n':Cmd.NEW}
CMD_CHARS = ''.join(CMD_DICT.keys())
    
    


#print(DESCRIPTION)
parser = argparse.ArgumentParser(description = DESCRIPTION, formatter_class=argparse.RawDescriptionHelpFormatter, prefix_chars='-+')
parser.add_argument("filename")
parser.add_argument("--make-cmd", help="make command to be executed")
# group = parser.add_mutually_exclusive_group()
# # parser.add_argument_group("Specifying default behaviour")
# group.add_argument("+m", action="store_true")
# #group.add_argument("+q", action="store_true")
# group.add_argument("+c", action="store_true")
# group.add_argument("+g", action="store_true")
args = parser.parse_args()

# defaultCmd = {
#     "isMake" : args.m, 
#     "isQuit" : args.q,
#     "isClipboard" : args.c,
#     "isGenerate": args.g
# }
filename = args.filename
makeCmd = args.make_cmd


quitAtEnd = True # defaultCmd["isQuit"]



options = webdriver.FirefoxOptions()
options.unhandled_prompt_behavior = 'ignore'
browser = None 

def ctrlKey(actions,key):
    return actions.key_down(Keys.CONTROL).send_keys(key).key_up(Keys.CONTROL)




def parseMagic(line):
    # searchPrefix = r"^(.*)" + MAGIC_STRING + r" *((?:[-+][ngecmq]+ *)+)"
    searchPrefix = r"^(.*)" + MAGIC_STRING + r" *\+([" + CMD_CHARS + "])"
    search = re.search(searchPrefix + r" (.*)$", line)
    if not search:
        # in this case, content is empty
        search = re.search(searchPrefix + r"(.*)$", line)

    if search:
        prefix = search.group(1)
        command = CMD_DICT[search.group(2)]
        #command =    ''.join([x[1:] for x in commands if x[0] == '+'])
        #negcommand = ''.join([x[1:] for x in commands if x[0] == '-'])        
        content = search.group(3).strip()
        return Command(content=content, prefix=prefix, command=command)
    else:
        return None

def makeDiag(cmd):
      global quitAtEnd
      content = cmd.content
    #   command = cmd.command
      # negcommand = cmd.negcommand
  
    #   isNew = 'n' in command
    #   isEdit = 'e' in command
    #   def makeFlag(char, key):
    #     return (defaultCmd[key] and char not in negcommand) or char in command
    #   isGenerate = makeFlag('g', "isGenerate")
    #   isClipboard = makeFlag('c', "isClipboard")
    #   isMake = makeFlag('m', "isMake")      
    #   quitAtEnd = makeFlag('q', "isQuit")
      return (Diag(content= content,
                  # prefix = prefix,
                  isFile = len(content) > 0 and (content[0] != '{'),
                  command = cmd.command
               ))         

def listDiags(filename):
    # Opening file
    file = open(filename, 'r')
    diags = []
# Closing files

    for line in file:           
           search = parseMagic(line)
           
           if search:                            
               diags.append(makeDiag(search))         
    file.close()
    return diags

def loadEditor(diag):
  browser.get(URL_YADE)
  stripped_diag = diag.strip()
  print("debut" + stripped_diag + "fin")
  if (stripped_diag.strip() == ""):
      print("Creating new diagram")
      browser.find_element(By.XPATH, '//button[text()="Clear"]').click()
  else:
      print("Loading diagram ")
      cmd = 'sendGraphToElm('+ stripped_diag + ", 'python');"
      browser.execute_script(cmd)

def waitSaveDiag():
  
  # browser.execute_script('var

  print("Save to continue")
  #loadButton = browser.find_element(By.XPATH, '//button[text()="Save"]')

  def createSaveButton(text, key):
    browser.execute_script("""

  saveButton = document.createElement("button");
  saveButton.innerText = '{text}';
  saveButton.addEventListener("click", function () {{ window.handleSave('{key}');}});
  var loadButton = document.getElementById('{idload}');
  loadButton.parentNode.insertBefore(saveButton, loadButton);
  """.format(text=text, idload = ID_LOAD_BUTTON, key = key))



  createSaveButton('Save & Exit', EXIT_KEY)
  createSaveButton('Save & make Latex', MAKE_KEY)
  createSaveButton('Save & copy Latex', COPY_KEY)

  # browser.find_element(By.ID, "save-button").click()
  browser.execute_script('document.styleSheets[0].insertRule("#{idsave}  {{ display : none;}}", 0 );'.format(idsave=ID_SAVE_BUTTON))
  

 

  cmd = "window.handleSave = arguments[arguments.length - 1]; "
  key = None
  while key == None:
    try:
        key = browser.execute_async_script(cmd)
    except UnexpectedAlertPresentException:
        continue

  
   # browser.execute_script('document.getElementById("save-button").click()')


  # we unsubscribe the implemented saveGraph procedure 
  return (key, browser.execute_async_script('''     
     callback = arguments[arguments.length - 1];
     app.ports.saveGraph.unsubscribe(saveGraph);
     app.ports.saveGraph.subscribe(function(a) {{ callback(JSON.stringify(fromElmGraph(a))); }});
     document.getElementById('{idsave}').click();
  '''.format(idsave = ID_SAVE_BUTTON)))


def genLatex():
  print("Exporting to latex")
  # Restoring the usual saveGraph procedure
  browser.execute_script("app.ports.saveGraph.subscribe(saveGraph);")
  
  canvas = browser.find_element(By.ID, "canvas")
  canvas.click()
  actions = ActionChains(browser)
  actions.move_to_element(canvas).move_by_offset(10,10).perform()
  ctrlKey(actions,'a').perform()
  browser.find_element(By.XPATH, '//button[text()="Export selection to quiver"]').click()
  # switch to new tab
  WebDriverWait(browser, 20).until(EC.number_of_windows_to_be(2))
  browser.switch_to.window(browser.window_handles[1])
  
  WebDriverWait(browser, 5).until(EC.invisibility_of_element((By.CSS_SELECTOR, "div.loading-screen")))
  el=WebDriverWait(browser, 5).until(EC.element_to_be_clickable((By.CSS_SELECTOR,"button[title='LaTeX']")))
  WebDriverWait(browser, 5).until(EC.invisibility_of_element((By.CSS_SELECTOR, "div.loading-screen")))
  el.click()
  el=WebDriverWait(browser, 5).until(EC.presence_of_element_located((By.CSS_SELECTOR,"div.code")))
  latex = el.text
  return latex
    
# return the json-encoded graph and the generated latex
def handleDiag(diag):
    global browser
    if diag.isFile:        
        fileName = diag.content
    content = diag.content
    if diag.command == Cmd.NEW:
        if diag.isFile:
            if exists(fileName):
                print("Aborting diagram creation: File %s already exists." % fileName)
                exit(0)
            content = ''
        else:
            if content != "":
                print("Aborting diagram creation: data already provided (%s)." % content)
                exit(0)
    else:
        if diag.isFile:
           if not exists(fileName):
              print("Aborting diagram editing: file %s not found." % fileName)
              exit(0)
           
           file = open(fileName,mode='r')
           content = file.read()
           file.close()
        else:
            if content == "":
                print("Aborting diagram editing: no data provided.")
        
    
    

    # we only launch firefox once, if needed
    if browser == None:    
      browser = webdriver.Firefox(options=options)
      # long timeout for execute_async_script
      browser.set_script_timeout(84000)


    loadEditor(content)
    latex = None
    cmd = diag.command
    if diag.command.isInteractive():
       (key, content) = waitSaveDiag()       
       if key == MAKE_KEY:
          cmd = Cmd.MAKE
       if key == COPY_KEY:
          cmd = Cmd.COPY
       
    if cmd.isLatex():
       latex = genLatex()
    return (cmd, content, latex)

            



# does what is said in the help message
def remakeDiags(fileName):
    
    
    while True:
       diags = listDiags(fileName)
       if (len(diags) == 0):
          print("No diagram command found.")
          break
       print("%d diagram command(s) found" % len(diags))
       diag = diags[0]
       
       print("Handling a diagram")
       if not diag.command.isInteractive():
          print("- No user editing")
       if diag.command.isLatex():
          print("- Latex generation")
       if diag.isFile:
          print("- filename : " + diag.content)
       #print(diag)
       #exit(0)
       (cmd, graph, latex) = handleDiag(diag)

       if cmd == Cmd.COPY and latex:
          pyperclip.copy(latex)

       if diag.isFile:
           fileName = diag.content
           print("Writing to file " + fileName)
           f = open(fileName, "w")
           f.write(graph)
           f.close()

       with in_place.InPlace(filename) as fp:           
            done = False
            for line in fp:
                if done:
                    fp.write(line)
                else:
                    search = parseMagic(line)
                    if search == None:
                        fp.write(line)
                    else:
                        done = True
                        suffix = diag.content if diag.isFile else graph
                        fp.write(search.prefix + MAGIC_STRING + " " + suffix + "\n")
                        if cmd == Cmd.MAKE:
                            fp.write("\n% GENERATED LATEX\n")
                            fp.write(latex)
                            fp.write("\n% END OF GENERATED LATEX\n")
       if cmd == Cmd.MAKE and makeCmd:
            os.system(makeCmd)
        



remakeDiags(filename)
if quitAtEnd:
  if browser != None:
     browser.quit()
