FC        = gfortran
FFLAGS    = -fconvert=big-endian -frecord-marker=4 -ffree-line-length-none
DEPFLAGS  = -MMD -MP -I$(BUILD_DIR)

SRC_DIR   = src
TEST_DIR  = test
BUILD_DIR = build

TARGET = test.exe

SRCS      = $(wildcard $(SRC_DIR)/*.F90)
MAIN_SRCS = $(TEST_DIR)/main.f90

OBJS      = $(addprefix $(BUILD_DIR)/, $(notdir $(SRCS:.F90=.o)))
MAIN_OBJ  = $(BUILD_DIR)/main.o

DEPS      = $(OBJS:.o=.d) $(MAIN_OBJ:.o=.d)

all: $(TARGET)
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo "Successfully created $(TARGET) binary file"
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"

$(BUILD_DIR):
	@mkdir -p $@

$(TARGET): $(OBJS) $(MAIN_OBJ)
	$(FC) $(FFLAGS) -o $@ $^

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.F90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) $(DEPFLAGS) -c $< -o $@ -J$(BUILD_DIR)

$(BUILD_DIR)/%.o: $(TEST_DIR)/%.F90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) $(DEPFLAGS) -c $< -o $@ -J$(BUILD_DIR)

$(BUILD_DIR)/%.o: $(BUILD_DIR)/%.d
$(BUILD_DIR)/%.d: ;

-include $(DEPS)

clean:
	rm -rf $(BUILD_DIR) $(TARGET)

.PHONY: all clean