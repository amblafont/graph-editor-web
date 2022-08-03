#print(" Pour que ça marche il faut firefox et ajouter geckodriver soit dans PATH")
print("This python3 script requires the selenium geckodriver (https://selenium-python.readthedocs.io/installation.html#drivers)")
print("Other requirements: the in_place package")
print()



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

Diag = namedtuple('Diag' , 'content isFile isEdit isNew isLatex isGenerate isClipboard isMake')
Command = namedtuple('Command' , 'content prefix command')

import sys
import re
import in_place
import argparse

MAGIC_STRING = "YADE DIAGRAM"
URL_YADE = os.getenv('YADE_URL')
if not URL_YADE:
    URL_YADE = 'https://amblafont.github.io/graph-editor/index.html'

DESCRIPTION = """
This script looks for lines ending with YADE DIAGRAM 'commands [diagram]")

- [diagram] (optional) is a filename, or a json encoded diagram (pasted from the editor by 
  using C-c  on some selected diagram, or from the content of a saved file).
- "commands" is a string where each char has a specific meaning (some of them are incompatible):
    GENERAL COMMANDS
    * n : new diagram (which is edited)
    * e : edit diagram
          The edited diagram is saved either as a json encoded diagram or in the specified filename
    * q : quit browser at the end of script
    * m : make using the provided make command at 
    LATEX COMMANDS (executed after the user has saved the diagram, unless no editing was required)
    * g : latex generation inserted after the matching line    
    * c : latex copied to the clipboard    
"""
#print(DESCRIPTION)
parser = argparse.ArgumentParser(description = DESCRIPTION, formatter_class=argparse.RawDescriptionHelpFormatter)
parser.add_argument("filename")
parser.add_argument("-m", "--make", help="Make command")
args = parser.parse_args()

filename = args.filename
makeCmd = args.make



quitAtEnd = False



options = webdriver.FirefoxOptions()
options.unhandled_prompt_behavior = 'ignore'
browser = None 

def ctrlKey(actions,key):
    return actions.key_down(Keys.CONTROL).send_keys(key).key_up(Keys.CONTROL)




def parseMagic(line):
    searchPrefix = r"^(.*)" + MAGIC_STRING + r" *'([ngecmq]+)"
    search = re.search(searchPrefix + r" +(.*)$", line)
    if not search:
        # in this case, content is empty
        search = re.search(searchPrefix + r"(.*)$", line)

    if search:
        prefix = search.group(1)
        command = search.group(2)
        content = search.group(3)
        return Command(content=content, prefix=prefix, command=command)
    else:
        return None

def makeDiag(cmd):
      global quitAtEnd
      content = cmd.content
      command = cmd.command
      isNew = 'n' in command
      isEdit = 'e' in command
      isGenerate = 'g' in command
      isClipboard = 'c' in command
      isMake = 'm' in command
      quitAtEnd = quitAtEnd or 'q' in command
      return (Diag(content= content,
                  # prefix = prefix,
                  isFile = len(content) > 0 and (content[0] != '{'),
                  isEdit = isEdit or isNew,
                  isNew = isNew,
                  isGenerate = isGenerate,
                  isLatex = isClipboard or isGenerate,
                  isClipboard = isClipboard,
                  isMake = isMake
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
  if (diag.strip() == ""):
      print("Creating new diagram")
      browser.find_element(By.XPATH, '//button[text()="Clear"]').click()
  else:
      print("Loading diagram ")
      cmd = 'sendGraphToElm('+ diag + ", 'python');"
      browser.execute_script(cmd)
def waitSaveDiag():
    # we unsubscribe the implemented saveGraph procedure 
  browser.execute_script("app.ports.saveGraph.unsubscribe(saveGraph);")
  # browser.execute_script('var

  print("Save to continue")  
  # graph = None: it may be that the script was interrupted because
  # of a modal dialog box
  cmd =  "var mysave = function(a) {  window.saveSelenium(JSON.stringify(fromElmGraph(a))); } ;" 
  cmd = cmd + 'app.ports.saveGraph.subscribe(mysave);'
  browser.execute_script(cmd)

  cmd = "window.saveSelenium = arguments[arguments.length - 1]; "
  graph = None
  while graph == None:
    try:
        graph = browser.execute_async_script(cmd)
    except UnexpectedAlertPresentException:
        continue
  return graph

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
    if diag.isNew:
        
        if diag.isFile:
            if exists(fileName):
                print("Aborting diagram creation: File %s already exists." % fileName)
                exit(0)
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
    if diag.isEdit:
       content = waitSaveDiag()
    if diag.isLatex:
       latex = genLatex()
    return (content, latex)

            



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
       if not diag.isEdit:
          print("- No user editing")
       if diag.isLatex:
          print("- Latex generation")
       if diag.isFile:
          print("- filename : " + diag.content)
       #print(diag)
       #exit(0)
       (graph, latex) = handleDiag(diag)

       if diag.isClipboard and latex:
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
                        if diag.isGenerate:
                            fp.write("\n% GENERATED LATEX\n")
                            fp.write(latex)
                            fp.write("\n% END OF GENERATED LATEX\n")
       if diag.isMake:
            os.system(makeCmd)
        



remakeDiags(filename)
if quitAtEnd:
  browser.quit()