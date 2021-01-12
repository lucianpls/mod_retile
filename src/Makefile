MODULE = mod_retile
include Makefile.lcl

C_SRC = $(MODULE).cpp
HEADERS =

FILES = $(C_SRC)
OBJECTS = $(FILES:.cpp=.lo)

CXXFLAGS = -prefer-pic -O2 -Wall
DEFINES = -DLINUX -D_REENTRANT -D_GNU_SOURCE $(DEBUG)

LIBS = -L$(DEST) -lahtse -lstdc++

# Create Makefile.lcl, which should define
# MOD_PATH, the folder where the apache module should reside
# INCLUDES, includes for httpd, apr headers
# For example look at Makefile.lcl.example

TARGET = .libs/$(MODULE).so

# Can't use apxs to build c++ modules
# The options used here might depend on how apache was built
$(TARGET)       :       $(OBJECTS)
	$(LIBTOOL) --mode=link g++ -o $(MODULE).la -rpath $(DEST) -module -avoid-version $^ $(LIBS)

%.lo	:	%.cpp $(HEADERS)
	$(LIBTOOL) --mode=compile g++ -std=c++0x $(CXXFLAGS) $(DEFINES) $(INCLUDES) -pthread -c -o $@ $< && touch $(@:.lo=.slo)

install : $(TARGET)
	sudo cp $(TARGET) $(DEST)

clean   :
	rm -rf .libs *.o *.lo *.slo *.la
