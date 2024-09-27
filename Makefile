LINK_FILES	:= .zshrc .emacs.d .vim .zplug
LINK_TARGETS	:= $(addprefix $(HOME)/, $(LINK_FILES))

.DEFAULT_GOAL := install

$(HOME)/%:
	ln -sf $(CURDIR)/$(@F) $@

$(HOME)/.vim:
	ln -sf $(CURDIR)/.vim $@
	cd $(CURDIR)/.vim && git submodule init && git submodule update
	vim +PluginInstall +qall

$(HOME)/.zplug:
	gh repo clone zplug/zplug $@

.PHONY: install
install: $(LINK_TARGETS)

.PHONY: clean
clean:
	@echo "Delete these files"
	@echo "$(LINK_TARGETS)"
	@echo "Can I delete it ?"
	@read -p "Hit enter: "
	rm -rf $(LINK_TARGETS)
