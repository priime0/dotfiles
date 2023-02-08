#include <X11/Xlibint.h>
#include <X11/extensions/Xrandr.h>
#include <stdio.h>

int main(int argc, char **args) {
  const char *display_name = ":0";
  Display *dpy = XOpenDisplay(display_name);
  Window window = XDefaultRootWindow(dpy);
  XRRMonitorInfo *monitor = XRRGetMonitors(dpy, window, 1, &dpy->nscreens);

  int is_laptop_screen = monitor->height == 1200 ? 1 : 0;
  FILE *fp = popen(
      "cat /proc/acpi/button/lid/LID0/state | grep -i closed | wc -l", "r");
  if (fp == NULL) {
    exit(1);
  }

  char buf[2];
  int is_closed = 0;
  if (fgets(buf, sizeof(buf), fp) != NULL) {
    is_closed = atoi(buf);
    pclose(fp);
  } else {
    exit(1);
  }

  if (is_closed && is_laptop_screen) {
    system("systemctl suspend");
  }

  printf("");

  return 0;
}
