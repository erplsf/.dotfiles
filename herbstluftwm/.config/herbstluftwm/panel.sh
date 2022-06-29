# Terminate already running bar instances
pkill polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch the bar
for m in $(polybar --list-monitors | cut -d":" -f1); do
	MONITOR=$m polybar -q default &
done
