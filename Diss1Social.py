# 11111111111111

# experiment 1

# Social First

# taken from demo
from __future__ import division
from psychopy import visual, core, event, gui

# add dialogue box for sub id and condition
myDlg = gui.Dlg(title="Diss 1")
myDlg.addField('Subject ID:')
myDlg.addField('Condition:')
ok_data = myDlg.show()  # show dialog and wait for OK or Cancel
if myDlg.OK:  # or if ok_data is not None
    subid = ok_data[0]
    cond = ok_data[1]
else:
    print('user cancelled')
    core.quit()


# the condition sets which instruction file is pulled
if cond.lower() == 'a':   
    insfile = "note_report_inst.txt"
elif cond.lower() == 'b':
    insfile = 'nonote_report_inst.txt'
elif cond.lower() == 'c':
    insfile = 'nonote_noreport.txt'
else:
    print('Invalid condtion')
    core.quit()
instr_txt = open(insfile).read().strip()       # get instructions

# create text for between movies and thank you at end
between = 'Please press the SPACE BAR when you are ready to continue with the next lecture.'
end = 'You are finished with the lectures\nPlease get the researcher to continue.'
 
# create window to draw in 
win = visual.Window((1024, 768))
# create a clock
globalClock = core.Clock()

# define movie stim Social 1st
mov1 = visual.MovieStim3(win, 'Seg.mov', size=(960, 540),
# mov1 = visual.MovieStim3(win, 'SegCogDis.mov', size=(960, 540),
    flipVert=False, flipHoriz=False, loop=False)

# Thinking 2nd
mov2 = visual.MovieStim3(win, 'Seg.mov', size=(960, 540),
# mov2 = visual.MovieStim3(win, 'SegThinking.mov', size=(960, 540),
    flipVert=False, flipHoriz=False, loop=False)

#
# present general instructions
instructions_txt = visual.TextStim(win, text=instr_txt, color='black', height = .08, wrapWidth = 1.8)
instructions_txt.draw()     # draw text
win.flip()                    # show it
event.waitKeys()            # wait for keypress
win.flip()                    # blank screen
core.wait(0.5)                  # pause for half a sec
#
responses1 = []
responses2 = []
outfile = open('data1.txt', mode = 'a')

#reset clock
globalClock.reset()
# start movie 1
while mov1.status != visual.FINISHED:
    mov1.draw()
    win.flip()
    keys = event.getKeys(keyList =['left','right'], timeStamped = globalClock)
    if len(keys) > 0:
        responses1.append(keys)

for response in responses1:
    outfile.write('%s\t%s\t%s\t%s\t%s\t%.3f\n' % (subid,cond,'ST','social',response[0][0],response[0][1]))

# present between lecture note
between_note = visual.TextStim(win, text=between, color='black', wrapWidth = 1.8)
between_note.draw()     # draw text
win.flip()                    # show it
event.waitKeys(keyList=['space'])            # wait for keypress
win.flip()                    # blank screen
core.wait(0.5)                  # pause for half a sec

#reset clock
globalClock.reset()
# start movie 2
while mov2.status != visual.FINISHED:
    mov2.draw()
    win.flip()
    keys = event.getKeys(keyList =['left','right'], timeStamped = globalClock)
    if len(keys) > 0:
        responses2.append(keys)

for response in responses2:
    outfile.write('%s\t%s\t%s\t%s\t%s\t%.3f\n' % (subid,cond,'ST','thinking',response[0][0],response[0][1]))

outfile.close()

# present end of lecture note
between_note = visual.TextStim(win, text=end, color='black', wrapWidth = 1.8)
between_note.draw()     # draw text
win.flip()                    # show it
event.waitKeys(keyList=['space'])            # wait for keypress
win.flip()                    # blank screen
core.wait(0.5)                  # pause for half a sec


win.close()
core.quit()

# The contents of this file are in the public domain.
