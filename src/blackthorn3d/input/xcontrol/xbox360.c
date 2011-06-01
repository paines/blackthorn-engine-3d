#include "XInput.h"
#include "xbox360.h"

void xbox360_vibrate(int controller, 
                     unsigned short speed_left, 
                     unsigned short speed_right) {

    XINPUT_VIBRATION vibration;
    vibration.wLeftMotorSpeed  = speed_left;
    vibration.wRightMotorSpeed = speed_right;
    XInputSetState(controller, &vibration);
}

XINPUT_STATE controller_state;

void xbox360_poll(int controller) {
    XInputGetState(controller, &controller_state);
}

int xbox360_get_a(int controller) {
    int value = controller_state.Gamepad.wButtons & XINPUT_GAMEPAD_A;
    return (value > 0);
}

int xbox360_get_b(int controller) {
    int value = controller_state.Gamepad.wButtons & XINPUT_GAMEPAD_B;
    return (value > 0);
}

int xbox360_get_x(int controller) {
    int value = controller_state.Gamepad.wButtons & XINPUT_GAMEPAD_X;
    return (value > 0);
}

int xbox360_get_y(int controller) {
    int value = controller_state.Gamepad.wButtons & XINPUT_GAMEPAD_Y;
    return (value > 0);
}

short xbox360_get_lx(int controller) { return controller_state.Gamepad.sThumbLX; }
short xbox360_get_ly(int controller) { return controller_state.Gamepad.sThumbLY; }
short xbox360_get_rx(int controller) { return controller_state.Gamepad.sThumbRX; }
short xbox360_get_ry(int controller) { return controller_state.Gamepad.sThumbRY; }

int xbox360_get_ltrig(int controller) { return controller_state.Gamepad.bLeftTrigger; }
int xbox360_get_rtrig(int controller) { return controller_state.Gamepad.bRightTrigger; }

int xbox360_get_lbump(int controller) {
    int value = controller_state.Gamepad.wButtons & XINPUT_GAMEPAD_LEFT_SHOULDER;
    return (value > 0);
}

int xbox360_get_rbump(int controller) {
    int value = controller_state.Gamepad.wButtons & XINPUT_GAMEPAD_RIGHT_SHOULDER;
    return (value > 0);
}
