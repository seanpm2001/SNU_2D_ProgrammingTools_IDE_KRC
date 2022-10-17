# makefile for Kent Recursive Calculator
# (BCPL version translated into C)

#alternatives
#PREFIX=/usr/local
PREFIX=/usr

BINDIR=$(PREFIX)/bin
LIBDIR="$(PREFIX)/lib/krc"
MANDIR=$(PREFIX)/share/man/man1

# To use alternate compilers, just go
#	CC=clang make clean all

# To set the default number of cells, use
#	HEAPSIZE=1000000 make clean all
# KRC will take 2*2*sizeof(pointer) times this amount of RAM:
# 16MB on a 32-bit machine, 32MB on a 64-bit machine.
# If you have a desktop system, aim for half the physical RAM.

HEAPSIZE?=128000		# Default heap size if unspecified

# -fno-omit-frame-pointer is necessary when optimizing with gcc or clang
# otherwise it makes an extra register available to functions which is
# NOT saved by setjmp/longjmp, so the garbage collector fails to update
# its contents, causing heap corruption.

# delete "-DLINENOISE" to remove line editing and command history,
# reduces code size from 75K to 63K

CFLAGS+=-g -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast \
	-O2 -fno-omit-frame-pointer \
 	-DLINENOISE \
	-DLIBDIR='$(LIBDIR)' -DHEAPSIZE=$(HEAPSIZE)

SRCS= main.c reducer.c compiler.c lex.c listpack.c bcpl.c emas.c \
      listhdr.h comphdr.h redhdr.h bcpl.h emas.h \
      linenoise.h linenoise.c
OBJS= main.o reducer.o compiler.o lex.o listpack.o bcpl.o emas.o \
      linenoise.o

krc: $(OBJS)
	@$(CC) $(CFLAGS) -o $@ $(OBJS)

listpack.o: listhdr.h bcpl.h
lex.o:      comphdr.h listhdr.h bcpl.h emas.h
compiler.o: comphdr.h listhdr.h bcpl.h
reducer.o:  redhdr.h comphdr.h listhdr.h bcpl.h
main.o:     redhdr.h comphdr.h listhdr.h bcpl.h emas.h revision

linenoise.o: Makefile
main.o: Makefile
listpack.o: listhdr.h bcpl.h

.c.o:
	@$(CC) $(CFLAGS) -c $<

.c.s:
	@$(CC) $(CFLAGS) -S $<

install: krc krclib/prelude krclib/lib1981 doc/krc.1
	install -d -m 755 $(BINDIR) $(LIBDIR) $(LIBDIR)/help $(MANDIR)
	install -s -m 755 krc $(BINDIR)
	install -c -m 644 krclib/prelude $(LIBDIR)
	install -c -m 644 krclib/lib1981 $(LIBDIR)
#	install -c -m 644 krclib/help/* $(LIBDIR)/help
	cp -P krclib/help/* $(LIBDIR)/help  #krclib/help contains symbolic links
	install -c -m 644 doc/krc.1 $(MANDIR)

uninstall: 
	rm -rf $(BINDIR)/krc $(LIBDIR) $(MANDIR)/krc.1

clean:
	@rm -f *.o *.s core
	make -C doc $@
