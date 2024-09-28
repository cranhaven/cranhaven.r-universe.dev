ZLIBINC = ../include

LIBPNGINC = ../include

FTINC = ../include/freetype

CP = cp -fp
RM = rm -f

AR_RC = $(AR) rcs

ACTCMP_OBJS = actioncompiler/assembler.o actioncompiler/compile.o \
	actioncompiler/compileaction.o actioncompiler/lex.swf4.o \
	actioncompiler/lex.swf5.o actioncompiler/listaction.o \
	actioncompiler/swf4compiler.tab.o actioncompiler/swf5compiler.tab.o

BLOCKS_OBJS = blocks/action.o blocks/binary.o blocks/bitmap.o \
	blocks/block.o blocks/browserfont.o blocks/button.o \
	blocks/character.o blocks/cxform.o blocks/dbl.o \
	blocks/error.o blocks/fdbfont.o blocks/fileattrs.o \
	blocks/fillstyle.o blocks/filter.o blocks/flv.o \
	blocks/font.o blocks/fromswf.o blocks/gifdbl.o \
	blocks/gradient.o blocks/imports.o blocks/input.o \
	blocks/jpeg.o blocks/linestyle.o blocks/matrix.o \
	blocks/metadata.o blocks/method.o blocks/morph.o \
	blocks/mp3.o blocks/output.o blocks/outputblock.o \
	blocks/placeobject.o blocks/pngdbl.o blocks/protect.o \
	blocks/rect.o blocks/scalinggrid.o blocks/scenedata.o \
	blocks/scriptlimits.o blocks/shape.o blocks/sound.o \
	blocks/soundinstance.o blocks/soundstream.o blocks/sprite.o \
	blocks/symbolclass.o blocks/tabindex.o blocks/text.o \
	blocks/textfield.o blocks/ttffont.o blocks/utf8.o \
	blocks/videostream.o

OBJS = blocklist.o compat.o displaylist.o fill.o font_util.o \
	gc.o ming.o ming_def.o movie.o movieclip.o \
	position.o shape_cubic.o shape_util.o text_util.o


.PHONY: actcmp blks all clean

all: libming.a

libming.a: actcmp blks $(OBJS)
	@$(RM) $@
	$(AR_RC) $@ $(ACTCMP_OBJS) $(BLOCKS_OBJS) $(OBJS)

actcmp:
	cd actioncompiler && $(MAKE) all CC="$(CC)" CFLAGS="$(CFLAGS)" CPPFLAGS="$(CPPFLAGS)"

blks:
	cd blocks && $(MAKE) all CC="$(CC)" CFLAGS="$(CFLAGS)" CPPFLAGS="$(CPPFLAGS)"

.c.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) -I$(ZLIBINC) -I. -c $< -o $@

clean:
	cd actioncompiler && $(MAKE) clean
	cd blocks && $(MAKE) clean
	$(RM) *.a
	$(RM) *.o
