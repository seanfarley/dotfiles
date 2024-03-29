zmodload -F zsh/stat b:zstat

function print_all_colors() {
  for i in {0..255}
  do
    print -Pn "%K{$i}  %k%F{$i}${(l:3::0:)i}%f " ${${(M)$((i%6)):#3}:+$'\n'}
  done
}

function prompt_smf_user() {
  local name="${USER}"
  if [[ "$name" == "root" ]]
  then
    p10k segment -i '󱅞' -f 196 -t "%B${name}%b"
  else
    p10k segment -i '' -f red -t "${name}"
  fi
}

function prompt_smf_hostname() {
  local hn="$(hostname -s 2>/dev/null || hostname 2>/dev/null)"
  local hn_color=076

  if [[ "$OSTYPE" == "linux"* ]]
  then
    if [[ -f /etc/os-release ]]
    then
      # disabling since the warning is a false positive
      # shellcheck disable=SC1091
      source /etc/os-release
      OS="$NAME"
      VER="$VERSION_ID"

      # some versions of debian don't have VERSION_ID
      [[ -z "$VER" ]] && VER="$(uname -r)"
    else
      # Fall back to uname, e.g. "Linux <version>", also works for BSD, etc.
      OS=$(uname -s)
      VER=$(uname -r)
    fi
  elif [[ "$OSTYPE" == "darwin"* ]]
  then
    VER=$(echo "$OSTYPE" | cut -d n -f 2 | cut -d . -f 1)
    case $VER in
      23) VER="Sonoma";;
      22) VER="Ventura";;
      21) VER="Monterey";;
      20) VER="Big Sur";;
      19) VER="Catalina";;
      18) VER="Mojave";;
    esac
  else
    VER="$OSTYPE"
  fi

  # color based on hostname
  case $hn in
    laptop) hn_color=135;;
    theta*) hn_color=002;;
    minimac*|gcc*) hn_color=013;;
    euclid*) hn_color=172;;
    cloud*) hn_color=247;;
    box*) hn_color=220;;
    matrix*) hn_color=075;;
    retropi*) hn_color=226;;
    glados*) hn_color=160;;
  esac

  # remove spaces
  VER=${VER//[[:blank:]]/}
  # the double comma lowercases everything
  # [[ $PROMPT_SHOW_OS_VERSION != 0 ]] && pver=" (${VER,,})"

  p10k segment -f "$hn_color" -t "$hn (${(L)VER})"
}

function prompt_smf_os_icon() {
  local hn="$(hostname -s 2>/dev/null || hostname 2>/dev/null)"
  local color_host=163

  # color icons based on their logos
  case $_p9k_os_icon in
    **) color_host=247;;
    **)
      color_host=172
      [[ "$hn" == *"retropie"* ]] && _p9k_os_icon=""
      ;;
    **) color_host=126;;
    **) color_host=198;;
    **) color_host=196;;
    **) color_host=075;;
    **) color_host=162;;
    **) color_host=040;;
    **) color_host=228;;
    **)
      color_host=007
      # check is SUSE
      if [[ $(grep -c SUSE /etc/os-release) != 0 ]]; then
        _p9k_os_icon=""
        color_host=040
      fi
      ;;
  esac
  p10k segment -f "$color_host" -t "$_p9k_os_icon"
}

function prompt_smf_shlvl() {
  [[ $SHLVL -gt 1 ]] && p10k segment -f 214 -i "⟱️" -t "$SHLVL"
}
