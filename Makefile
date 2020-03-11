# Paths
SRCDIR   := src
OBJDIR   := build
GP       := gpascal
REU      := reu
BOOT     := boot
GP_SRC   := $(SRCDIR)/$(GP).s
GP_OBJ   := $(OBJDIR)/$(GP).o
GP_BIN   := $(OBJDIR)/$(GP).bin
GP_LBL   := $(OBJDIR)/$(GP).labels
GP_PRG   := $(OBJDIR)/$(GP).prg
REU_SRC  := $(SRCDIR)/$(REU).s
REU_LBL  := $(OBJDIR)/$(REU).labels
REU_PRG  := $(OBJDIR)/$(REU).prg
BOOT_SRC := $(SRCDIR)/$(BOOT).bas
BOOT_PRG := $(OBJDIR)/$(BOOT).prg
PRG      := $(GP_PRG) $(REU_PRG) $(BOOT_PRG)
D64      := $(OBJDIR)/gpascal_reu.d64

# Commands
ASM_GP = ca65 -g -o $(GP_OBJ) $(GP_SRC)
LNK_GP = ld65 -Ln $(GP_LBL) -o $(GP_BIN) -t none -S 32768 $(GP_OBJ)
LNK_REU = cl65 -g -o $(REU_PRG) --start-addr '$$7F00' -t c64 -C c64-asm.cfg -Ln $(REU_LBL) $(REU_SRC)
PAK_GP = dd if=$(GP_BIN) bs=1 skip=88 of=0. &&\
printf "\000\200" >$(GP_PRG) &&\
cat 0. >>$(GP_PRG) && rm -f 0.
TOK_BOOT = petcat -o $(BOOT_PRG) -w2 $(BOOT_SRC)
RM := rm -rf
MKDIR := mkdir -p
MKD64 := c1541 -format "g-pascal reu",gp d64

# Targets
.PHONY: all d64 clean

all: d64

d64: $(PRG)
	$(MKD64) $(D64) -attach $(D64) $(foreach p,$(PRG),-write $(p) $(subst .prg,,$(subst build/,,$(p))))

$(BOOT_PRG): $(BOOT_SRC)
	$(TOK_BOOT)

$(GP_OBJ): $(GP_SRC)
	$(ASM_GP)

$(GP_BIN): $(GP_OBJ)
	$(LNK_GP)

$(GP_PRG): $(GP_BIN)
	$(PAK_GP)

$(REU_PRG): $(REU_SRC)
	$(LNK_REU)

$(GP_SRC): | $(OBJDIR)
$(REU_SRC): | $(OBJDIR)

$(OBJDIR):
	$(MKDIR) $(OBJDIR)

clean:
	$(RM) $(OBJDIR)
