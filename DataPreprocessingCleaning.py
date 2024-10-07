import os
import rclone

os.system('pip install rclone')
os.system('rclone config')
os.system('rclone copy /content/drive/MyDrive filepath')

