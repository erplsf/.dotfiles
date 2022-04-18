# Set up gpg-agent for signing stuff (seems to be required on macos)
if [ -z "$(pgrep gpg-agent)" ]; then
    eval $(gpg-agent --daemon)
fi
