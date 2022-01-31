struct device {
    unsigned long ID;
    char address[16];
    char subnet[16];
    char gateway[16];
    char MAC[18];
    char version[6];
    char date[11];
};


char *errmsg();
int   get_device(unsigned id, struct device *);

