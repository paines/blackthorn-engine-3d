#pragma once

void xbox360_vibrate(int controller, 
             unsigned short speed_left, 
             unsigned short speed_right);

void xbox360_poll(int controller);
int xbox360_get_a(int controller);
int xbox360_get_b(int controller);
int xbox360_get_x(int controller);
int xbox360_get_y(int controller);

short xbox360_get_lx(int controller);
short xbox360_get_ly(int controller);
short xbox360_get_rx(int controller);
short xbox360_get_ry(int controller);

int xbox360_get_ltrig(int controller);
int xbox360_get_rtrig(int controller);
