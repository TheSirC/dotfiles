#! /usr/bin/env nix-shell
#! nix-shell -i bash -p murmur mumble

set -euox pipefail
# PAID = PulseAudio ID
# ID = PID / process ID

function main () {
    if [ ! -e ./.mic_over_mumble ]; then
        mkdir ./.mic_over_mumble
    fi
    cd ./.mic_over_mumble

    rm murmur.ini || true
    echo "bonjour=true" >> murmur.ini
    echo "bandwidth=130000" >> murmur.ini

    if [ ! -e ./initdone ]; then
        echo "You will need to configure Mumble client to use the lowest possible latency."
        echo "We will start Mumble now. Please complete setup wizard and go to settings to increase quality and decrease latency."
        echo "Also, mute your microphone (if you have one) in Mumble."
        echo "Then close Mumble."
        run_mumble_client_wizard
        touch ./initdone
    fi

    echo "Starting Mumble server (murmurd)..."
    run_mumble_server_bg
    sleep 5
    echo "Starting Mumble client..."
    MUMBLE_CLIENT_ID=$(run_mumble_client_bg)
    sleep 15
    echo "Fetching PulseAudio configuration..."
    MUMBLE_CLIENT_PAID=$(get_mumble_client_paid)

    echo "Changing PulseAudio configuration..."
    echo "Adding sink..."
    SINK_MODULE_PAID=$(add_sink)
    sleep 3

    echo "Fetching current configuration to redirect Mumble..."
    SINK_PAID=$(get_sink_paid)
    pacmd move-sink-input "$MUMBLE_CLIENT_PAID" "$SINK_PAID"

    echo "Adding a virtual microphone..."
    SOURCE_MODULE_PAID=$(add_source)

    echo "Done. Please use pavucontrol to ensure everything works."
    echo "Press Return to shut down..."
    read -n1 -s -r
    echo "Shutting down..."

    echo "Stopping Mumble client..."
    kill -KILL "$MUMBLE_CLIENT_ID" || true
    sleep 2
    echo "Stopping Mumble server..."
    # TODO: find a better way to kill murmurd
    # (it forks, so we cannot use its PID)
    pkill murmurd || true
    echo "Restoring PulseAudio configuration..."
    pactl unload-module "$SOURCE_MODULE_PAID"
    pactl unload-module "$SINK_MODULE_PAID"
}

function run_mumble_client_wizard () {
    mumble >/dev/null 2>&1
}

function run_mumble_client_bg () {
    MUMBLE_URL="mumble://localhost"
    mumble $MUMBLE_URL >/dev/null 2>&1 &
    echo $!
}

function run_mumble_server_bg () {
     murmurd -ini ./murmur.ini &
}

function get_mumble_client_paid () {
    pacmd list-sink-inputs |
        grep -F -e "index: " -e "media.name = " |
        cut_every_second_newline |
        grep -F -e "Mumble" |
        print_second_column
}

function add_sink () {
    pactl load-module \
        module-null-sink \
        sink_name=Loopback_of_Mumble \
        sink_properties=device.description=Loopback_of_Mumble
}

function add_source () {
    pactl load-module \
        module-virtual-source \
        source_name=VirtualMic \
        master=Loopback_of_Mumble.monitor \
        source_properties=device.description=VirtualMic
}

function get_sink_paid () {
    pacmd list-sinks |
        grep -F -e "index: " -e "name: " |
        cut_every_second_newline |
        grep -F -e "Loopback" |
        print_second_column
}

# https://serverfault.com/a/375098/449626
function cut_every_second_newline () {
    awk 'ORS=NR%2?" ":"\n"'
}

function print_second_column () {
    awk '{print $2}'
}

main

