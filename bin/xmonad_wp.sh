
#!/bin/bash


IMAGE_DIR="/home/sjunkin/Backgrounds"

#PIC=$(ls $IMAGE_DIR/* | shuf -n1)

PIC=$(find $IMAGE_DIR -regex ".*\.\(jpg\|png\)" | shuf -n1)
echo $PIC
feh --bg-fill "$PIC"

#overly fond of this particular image currently.
#feh --bg-fill $IMAGE_DIR/looking_towards_home_by_gate_to_nowhere.jpg 
