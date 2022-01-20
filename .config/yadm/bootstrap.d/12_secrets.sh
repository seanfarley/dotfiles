#!/bin/sh

system_type=$(uname -s)
is_sudo=$(sh -c "sudo -vn 2>&1 && echo password" | grep -c password)

if [ "$system_type" = "Darwin" ] && [ $is_sudo = 1 ]; then

    echo "Configuring bitwarden server"

    [[ "$(bw config server)" == *"bitwarden.farley.io"* ]] || bw config server "https://bitwarden.farley.io"

    bw login --check &> /dev/null || bw login sean@farley.io "$(/usr/bin/security find-internet-password -w -a sean@farley.io -s bitwarden.farley.io)"

    if [ ! -f ~/.ssh/id_rsa ]; then
        bw --session "$(bw unlock $(/usr/bin/security find-internet-password -w -a sean@farley.io -s bitwarden.farley.io) --raw)" get notes id_rsa > ~/.ssh/id_rsa
        chmod 600 ~/.ssh/id_rsa
    fi

fi

# all hosts get our public key
pubkey="ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCZZq/kY+fvnF+hhrrhtbphIgRGPJv2MoI2162S15Qo87le7qAAjci9EnEOqPphrocerATm9NPQ8RpLXwnG0MX6qn/3tlBYWhzVNJ4y8I4LMNQw1hmzUALbeDqA7iSzFFDnsZv1Cl9yQLpD8HWtke1Y5sz61lh32YQ4HzUryjRX48hHdup7511V5LsutMLMT4nu8ICuoke9bjZOh75/yxA0ClTGKfeHvobN/VXHGVStI527mjSZvmKuaFIITeSFXpzGDOh2zZxreJ33CBy9hBA6nYrHy9rNicU1c4boFGKpRi7d1tOW55QP/lg7U9BxmKPFFoLdn2cSoJOm1Q4Jt32v sean@seanfarley.local"

if [ ! -f ~/.ssh/id_rsa.pub ]; then
    echo "$pubkey" > ~/.ssh/id_rsa.pub
    chmod 600 ~/.ssh/id_rsa.pub
fi

if [ ! -f ~/.ssh/authorized_keys ]; then
    echo "$pubkey" > ~/.ssh/authorized_keys
    chmod 600 ~/.ssh/authorized_keys
fi
