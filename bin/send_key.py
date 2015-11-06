#some libvirt vms mess keys up - this will send the vm in lookup by name the key sequence..y
import libvirt
conn = libvirt.open("qemu:///system")
dom = conn.lookupByName('corporate_vm')
#29 - LEFT_CTRL, 56 - LEFT_ALT, 111 - DELETE
dom.sendKey(0, 0, [29, 56, 111], 3, 0)
