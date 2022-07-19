#print(" Pour que ça marche il faut firefox et ajouter geckodriver soit dans PATH")
print("This python3 script requires the selenium geckodriver (https://selenium-python.readthedocs.io/installation.html#drivers)")
print("Other requirements: the in_place package")
print()

urlYade = 'https://amblafont.github.io/graph-editor/index.html'

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

import sys
import re
import in_place


if(len(sys.argv) != 2):
    print('Usage: %s filename' % sys.argv[0])
    print()
    print("This scripts looks for lines of the shape '% YADE DIAGRAM: {json-encoded-diagram}'")
    print("If such a line is not followed by '% GENERATED LATEX', then the script loads the diagram in the diagram editor, "\
           + " and wait for the user to save the diagram in order to generate latex which is then inserted in the file.")
    print()
    print("The json encoded diagram can be created by using the copying command (C-c) in an existing diagram, or by copying the content of a saved file.")
    print("A fresh diagram can also be created with a line '% YADE DIAGRAM: '")

    exit(0)

filename = sys.argv[1]


options = webdriver.FirefoxOptions()
# let the user handle modal dialog boxes when executing async js scripts
options.unhandled_prompt_behavior = 'ignore'
browser = None 

def ctrlKey(actions,key):
    return actions.key_down(Keys.CONTROL).send_keys(key).key_up(Keys.CONTROL)

# return the json-encoded graph and the generated latex
def editDiag(diag):
  global browser

  # we only launch firefox once, if needed
  if browser == None:    
    browser = webdriver.Firefox(options=options)

  browser.get(urlYade)
  if (diag.strip() == ""):
      print("Creating new diagram")
      browser.find_element(By.XPATH, '//button[text()="Clear"]').click()
  else:
      print("Loading diagram ")
      cmd = 'sendGraphToElm('+ diag + ", 'python');"
      browser.execute_script(cmd)
  # we unsubscribe the implemented saveGraph procedure 
  browser.execute_script("app.ports.saveGraph.unsubscribe(saveGraph);")
  # browser.execute_script('var

  print("Save to continue")
  graph = None
  # graph = None: it may be that the script was interrupted because
  # of a modal dialog box
  cmd = "var callback = arguments[arguments.length - 1]; " \
   "var mysave = function(a) {  callback(JSON.stringify(fromElmGraph(a))); } ;" \
   'app.ports.saveGraph.subscribe(mysave);'
  while graph == None:
    try:
       graph = browser.execute_async_script(cmd)
    except UnexpectedAlertPresentException:
        continue

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
  return (graph, latex)


# does what is said in the help message
def remakeDiags(filename):

    while True:
       # counting (incomplete) diagrams
       with in_place.InPlace(filename) as fp:
         nbDiags = 0
         nbIncompletes = 0
         # was there a diagram on the previous line?
         isDiag = False
         for line in fp:
           fp.write(line)
           if isDiag:
              search = re.search(r"^ *% *GENERATED LATEX.*$", line)
              if not search:
                  nbIncompletes = nbIncompletes + 1
           search = re.search(r"^( *% *YADE DIAGRAM:)(.*)$", line)
           if search:
               isDiag = True
               nbDiags = nbDiags + 1
           else:
               isDiag = False
         print("%d diagram(s) found (%d incomplete)" % (nbDiags,nbIncompletes))

       # have we found the first incomplete diagram
       found = False
       # was there a diagram on the previous line, or are we done (finding an incomplete diagram)?
       # diag is the first incomplete diagram
       diag = None
       with in_place.InPlace(filename) as fp:
         for line in fp:
           fp.write(line)
           if found:
               continue
           if diag != None:
              search = re.search(r"^ *% *GENERATED LATEX.*$", line)
              if search:
                  diag = None
              else:
                  found = True
           search = re.search(r"^( *% *YADE DIAGRAM:)(.*)$", line)
           if search:
               diag = search.group(2)

       if not found:
             break

       (newDiag, latex) = editDiag(diag)

       with in_place.InPlace(filename) as fp:
            diag = None
            previousLine = ""
            prefix = ""
            def treatLine(line):
                nonlocal diag, previousLine, prefix, latex, newDiag 
                if diag == None:
                   search = re.search(r"^( *% *YADE DIAGRAM:)(.*)$", line)

                   if search:
                       previousLine = line
                       diag = search.group(2)
                       prefix = search.group(1)
                   else:
                       fp.write(line)
                   return False
                else:
                   search = re.search(r"^ *% *GENERATED LATEX.*$", line)
                   diag = None
                   if search:
                       fp.write(previousLine)
                       fp.write(line)
                       return False
                   else:
                       fp.write(prefix + newDiag)
                       fp.write("\n% GENERATED LATEX (remove this line to regenerate/edit)\n")
                       fp.write(latex)
                       fp.write("\n% END OF GENERATED LATEX\n")
                       fp.write(line)
                       return True
            done = False
            for line in fp:
                if done:
                    fp.write(line)
                else:
                    done = treatLine(line)
            treatLine('')



remakeDiags(filename)
