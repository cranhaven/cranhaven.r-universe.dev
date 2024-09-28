ZLIBINC = ../../include

LIBPNGINC = ../../include

FTINC = ../../include/freetype

CP = cp -fp
RM = rm -f

OBJS = action.o binary.o bitmap.o block.o browserfont.o \
	button.o character.o cxform.o dbl.o error.o \
	fdbfont.o fileattrs.o fillstyle.o filter.o flv.o \
	font.o fromswf.o gifdbl.o gradient.o \
	imports.o input.o jpeg.o linestyle.o matrix.o \
	metadata.o method.o morph.o mp3.o output.o \
	outputblock.o placeobject.o pngdbl.o protect.o rect.o \
	scalinggrid.o scenedata.o scriptlimits.o shape.o sound.o \
	soundinstance.o soundstream.o sprite.o symbolclass.o tabindex.o \
	text.o textfield.o ttffont.o utf8.o videostream.o


.PHONY: all clean

all: $(OBJS)

.c.o:
	$(CC) $(CFLAGS) -I$(ZLIBINC) -I$(LIBPNGINC) -I$(FTINC) $(CPPFLAGS) -I. -I.. -c $< -o $@

clean:
	$(RM) *.o
