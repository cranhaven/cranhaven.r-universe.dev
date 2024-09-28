ZLIBINC = ../../include

LIBPNGINC = ../../include

FTINC = ../../include/freetype

CP = cp -fp
RM = rm -f

OBJS = assembler.o compile.o compileaction.o \
	lex.swf4.o lex.swf5.o listaction.o \
	swf4compiler.tab.o swf5compiler.tab.o


.PHONY: all clean

all: $(OBJS)

.c.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) -I. -I.. -c $< -o $@

clean:
	$(RM) *.o
