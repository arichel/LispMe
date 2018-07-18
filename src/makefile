########################################################################
# Makefile for desktop LispMe system
########################################################################
.SUFFIXES : .c .h .o

########################################################################
# Options etc.
# define symbol DUMP_STEPS to create a log file containing VM transitions
########################################################################
NAME        = lispme
LIBRARY     = lib$(NAME).a
LIBRARY_LD  = lib$(NAME).so

CC          = gcc 
LNFLAGS     = -lm
#CCFLAGS     = -O2 -m64 -ggdb -ansi -Wall -rdynamic -fPIC -DMAIN_REPL #-DDUMP_STEPS
CCFLAGS     = -m64 -ggdb -ansi -Wall -DMAIN_REPL #-DDUMP_STEPS

########################################################################
# File lists
########################################################################
OBJECTS     = $(NAME).o store.o error.o comp.o \
              numeric.o vm.o io.o

########################################################################
# Compile and link all
########################################################################
all:    $(NAME)

lib:    $(LIBRARY)

dynlib: $(LIBRARY_LD)

########################################################################
# Build LispMe
########################################################################
$(NAME):    $(OBJECTS)
	        @echo "\nBuilding test program $(NAME) ..."
	        $(CC) $(CCFLAGS) $(LNFLAGS) -o $(NAME) $(OBJECTS)
	        @rm $(OBJECTS)

$(LIBRARY): $(OBJECTS)
		@echo "\nBuilding static library $(LIBRARY) ..."
		@ar -cru $(LIBRARY) $(OBJECTS)
		@ranlib $(LIBRARY)
		@rm $(OBJECTS)

$(LIBRARY_LD): $(OBJECTS)
		@echo "Building shared library $(LIBRARY_LD) ..."
		$(CC) $(CCFLAGS) $(LNFLAGS) -shared \
		-o $(LIBRARY_LD) $(OBJECTS)
		@rm $(OBJECTS) 

########################################################################
# How to compile a C file
########################################################################
.c.o:
	$(CC) $(CCFLAGS) -c $<

########################################################################
# Clean up and leave only necessary files
########################################################################
clean :
	rm -f *.o *.bak
	rm -f $(NAME) $(LIBRARY) $(LIBRARY_LD)
