
"""
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
                 _       _              _        
                | |     | |           _| |       
  _____      _(_) |_ ___| |__    __ _(_)_|___  
 / __\ \ /\ / / | __/ __| '_ \  | '__| | __/ _ \ 
 \__ \| V  V /| | || (__| | | | | |  | | ||  __/ 
 |___/ \_/\_/ |_|\__\___|_| |_| |_|  |_|\__\___| 
                                                     
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #                                                     
"""
__author__  = "g_honk"
__credits__ = "n_conaway"
__status__  = "pilot"
__license__ = "GPL"

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #                                                     
from psychopy import visual, event, core, gui
from misc import *
from socket import gethostname
from time import strftime
import os, random as rnd, sys
import numpy as np
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #                                                     

# Experiment Settings
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #                                                     
experiment_name = 'switchrite'
conditions      = range(1,7)

##___________________________________
##              SHJ2    SHJ3    SHJ4|
## classify     1       2       3   |
## switch       4       5       6   |
##__________________________________|

# Cosmetics
text_color = [-1,-1,-1]
text_font  = 'Consolas'
text_size  = 22

image_start        = [0,150]
image_sizes        = [[195, 231],[130,154]] # size of images in pix
category_names     = ['Lape','Tannet']
feature_names_orig = ['Color','Veining','Shape']

## Stimuli Locations
if sys.platform == 'darwin' or sys.platform == 'linux2':
    image_directories = [os.getcwd() + '/img/one/', 
        os.getcwd() + '/img/two/',
        os.getcwd() + '/img/three/']
    subject_files = os.getcwd() + '/subjects/'
    feature_balance_list = np.genfromtxt(
        os.getcwd() + '/img/balancefeatures.csv', delimiter = ',',
            dtype = 'int').astype(int)
    dimension_balance_list = np.genfromtxt(
        os.getcwd() + '/img/balancedimensions.csv', delimiter = ',', 
            dtype = 'int').astype(int)
else:
    image_directories = [os.getcwd() + '\\img\\one\\', 
        os.getcwd() + '\\img\\two\\',
        os.getcwd() + '\\img\\three\\']
    subject_files = os.getcwd() + '\\subjects\\'
    feature_balance_list = np.genfromtxt(
        os.getcwd() + '\\img\\balancefeatures.csv', delimiter = ',',
            dtype = 'int').astype(int)
    dimension_balance_list = np.genfromtxt(
        os.getcwd() + '\\img\\balancedimensions.csv', delimiter = ',',
            dtype = 'int').astype(int)


# Get Subject Info and Start Window
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  # 
[subject_number, condition, subject_file] = get_subject_info(
    experiment_name, conditions, subject_files)

# Create Window and Set Logging Option
if gethostname() not in ['klab1','klab2','klab3']:
    win = visual.Window(fullscr = True, units = 'pix', color = [1,1,1], screen = 0) 
else:
    win = visual.Window(fullscr = True, units = 'pix',color = [1,1,1])
    check_directory(os.getcwd() + '\\logfiles\\')
    log_file = os.getcwd() + '\\logfiles\\' + str(subject_number)+ '-logfile.txt'
    while os.path.exists(log_file):
       log_file = log_file + '_dupe.txt'
    log_file = open(log_file,'w')
    sys.stdout = log_file
    sys.stderr = log_file


# Get Current Date and Time
current_time = strftime("%a, %d %b %Y %X")

# Start Mouse and Timer
cursor = event.Mouse(visible = True, newPos = None, win = win)
timer = core.Clock() #clock


# Load Stimuli and Assign Features
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  # 
stimuli = []
for i in image_directories:
    temp = []
    for j in os.listdir(i):
        if j[j.find('.'):] in ['.jpg','.png','.jpeg']:
            prop = str_2_prop(j[0:j.find('.')])
            stimuli.append ([
                visual.ImageStim(win, image = i + j, name = j, pos = image_start),
                j, prop])

# Counterbalance Dimensions
[stimuli, balance_condition, feature_names, dimension_assignment] = (
    counterbalance(subject_number, stimuli, feature_balance_list, 
        dimension_balance_list, feature_names_orig))

print '--------STIMULUS INFO--------'
for i in stimuli:
    print i

shj_types = [
#   Type Two
    [[[0,0,0],[0,0,1],[1,1,1],[1,1,0]],  
     [[0,1,0],[0,1,1],[1,0,0],[1,0,1]]],
#   Type Three  
    [[[0,0,0],[0,0,1],[0,1,0],[1,0,1]],
     [[1,1,1],[1,1,0],[0,1,1],[1,0,0]]],
#   Type Four
    [[[0,0,0],[0,0,1],[0,1,0],[1,0,0]],
     [[1,1,1],[1,1,0],[1,0,1],[0,1,1]]]]


# Set conditional variables by condition
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  # 

# Convert Condition to Specific Variables
if condition in [1, 4, 7]:
    shj_condition = 2
elif condition in [2, 5, 8]:
    shj_condition = 3
elif condition in [3, 6, 9]:
    shj_condition = 4
    
if condition <= 3:
    train_condition = 'classify'
else:
    train_condition = 'switchit'

valid_egs = shj_types[shj_condition - 2]

print '\n------------CONDITION INFO:------------'
print 'subject_number: ' + str(subject_number)
print 'exp_condition: ' + str(condition)
print 'training_condition: ' + train_condition
print 'shj_condition: ' + str(shj_condition)
print 'stim_condition: ' + str(balance_condition)

subject_data = [[current_time], [condition, subject_number, balance_condition]]


# Load Instructions
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
from instructs import *
instructions = visual.TextStim(win, text = '', wrapWidth = 2000, 
    color = text_color, font = text_font, height = text_size)
fix_cross = visual.TextStim(win, text = '+', pos = image_start, wrapWidth = 1000,
	color = text_color, font = text_font, height = text_size)

continue_string = '\n\
\n\
Click anywhere to continue.'


# Initiate Experiment Phases
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
num_training_blocks = 12

if train_condition == 'classify':
    if subject_number % 2 == 1: 
        execfile('train_classify.py')
        execfile('test_inference.py')
        execfile('test_switch.py')
        execfile('test_classify.py')
    else:
        execfile('train_classify.py')
        execfile('test_inference.py')
        execfile('test_classify.py')
        execfile('test_switch.py')
else:
    if subject_number % 2 == 1:
        execfile('train_switch.py')
        execfile('test_inference.py')
        execfile('test_switch.py')
        execfile('test_classify.py')        
    else:
        execfile('train_switch.py')
        execfile('test_inference.py')
        execfile('test_classify.py')
        execfile('test_switch.py')

# Exit screen
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
instructions.setText(instruction_text[-1])
instructions.draw()
win.flip()
event.waitKeys()

print '\nExperiment completed'
if gethostname() in ['klab1', 'klab2', 'klab3']:
    copy_2_db(subject_file, experiment_name)
    log_file.close()
    os.system("TASKKILL /F /IM pythonw.exe")