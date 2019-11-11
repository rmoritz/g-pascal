# Paths
SRCDIR := src
OBJDIR := build
SRC    := $(wildcard $(SRCDIR)/*.a)
OBJ    := $(SRC:$(SRCDIR)/%.a=$(OBJDIR)/%.o)
BIN    := $(OBJ:$(OBJDIR)/%.o=$(OBJDIR)/%.bin)
PRG    := $(BIN:$(OBJDIR)/%.bin=$(OBJDIR)/%.prg)
D64    := $(OBJDIR)/gpascal.d64

# Commands
ASM = ca65 -o $@ $<
LNK = ld65 -o $@ -t none -S 32768 $<
PAK = dd if=$< bs=1 skip=88 of=0. &&\
printf "\000\200" >$@ &&\
cat 0. >>$@ && rm -f 0.
RM := rm -rf
MKDIR := mkdir -p
MKD64 := c1541 -format gpascal,gp d64

# Rules
$(OBJDIR)/%.o: $(SRCDIR)/%.a
	$(ASM)

$(OBJDIR)/%.bin: $(OBJDIR)/%.o
	$(LNK)

$(OBJDIR)/%.prg: $(OBJDIR)/%.bin
	$(PAK)

# Targets
.PHONY: all d64 prg clean

all: d64
prg: $(PRG)

d64: $(PRG)
	$(MKD64) $(D64) -attach $(D64) $(foreach p,$(PRG),-write $(p) $(subst .prg,,$(subst build/,,$(p))))

$(SRC): | $(OBJDIR)

$(OBJDIR):
	$(MKDIR) $(OBJDIR)

clean:
	$(RM) $(OBJDIR)
