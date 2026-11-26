#!/usr/bin/env bash
# Estado de Exp_0 y de las ramas en los buckets de todas las VMs.
#
# Uso:  bash estado.sh BUCKET1 BUCKET2 ...
#       bash estado.sh              (lee los buckets de ~/dmeyf/buckets.txt, uno por linea)
#
# Necesita gcloud con permiso de lectura en cada bucket (una VM o Cloud Shell).
# Para cada bucket y experimento muestra las corridas terminadas (sp_* con
# PARAM.yml) y, de las que estan a medias, cuantas evaluaciones de la BO y
# semillas de produccion van y hace cuanto se subio algo (si son horas, la VM
# puede estar caida). Al final, el total por experimento juntando los buckets.

CORRIDAS=10; BO_TOTAL=12; SEMILLAS=10

if [ $# -gt 0 ]; then
  buckets=("$@")
elif [ -f ~/dmeyf/buckets.txt ]; then
  mapfile -t buckets < <(grep -v '^[[:space:]]*\(#\|$\)' ~/dmeyf/buckets.txt)
else
  echo "Uso: bash estado.sh BUCKET1 BUCKET2 ...  (o los buckets en ~/dmeyf/buckets.txt)"; exit 1
fi

ahora=$(date +%s)
hace() {  # fecha ISO -> "hace 3h 20m"
  local s=$(( ahora - $(date -d "$1" +%s) ))
  if [ $s -ge 3600 ]; then echo "hace $((s/3600))h $((s%3600/60))m"; else echo "hace $((s/60))m"; fi
}

declare -A terminadas  # experimento -> semillas terminadas (sin repetir entre buckets)

for b in "${buckets[@]}"; do
  b=${b#gs://}; b=${b%/}; b=$(echo "$b" | tr -d '[:space:]')
  echo "== gs://$b"
  # una sola consulta por bucket: tamano, fecha y ruta de cada archivo bajo exp/
  if ! listado=$(gcloud storage ls -l -r "gs://$b/exp/" 2>&1); then
    echo "   no se pudo leer: $(echo "$listado" | tail -1)"; continue
  fi
  listado=$(echo "$listado" | awk '$3 ~ /^gs:\/\// {print $2, $3}')  # fecha ruta
  exps=$(echo "$listado" | sed -nE "s#.* gs://$b/exp/([^/]+)/sp_[^/]+/.*#\1#p" | sort -u)
  [ -z "$exps" ] && { echo "   sin corridas"; continue; }

  for e in $exps; do
    sps=$(echo "$listado" | sed -nE "s#.* gs://$b/exp/$e/sp_([^/]+)/.*#\1#p" | sort -u)
    hechas=(); medias=()
    for sp in $sps; do
      if echo "$listado" | grep -q " gs://$b/exp/$e/sp_$sp/PARAM.yml$"; then hechas+=("$sp")
      else medias+=("$sp"); fi
    done
    terminadas[$e]+="${hechas[*]} "
    printf "   %-9s %2d terminadas" "$e" ${#hechas[@]}
    [ ${#hechas[@]} -gt 0 ] && printf " (sp %s)" "${hechas[*]}"
    echo
    for sp in "${medias[@]}"; do
      pre="gs://$b/exp/$e/sp_$sp"
      bo=0
      if echo "$listado" | grep -q " $pre/bo_evaluaciones.txt$"; then
        bo=$(( $(gcloud storage cat "$pre/bo_evaluaciones.txt" 2>/dev/null | wc -l) - 1 ))
        [ $bo -lt 0 ] && bo=0
      fi
      sem=$(echo "$listado" | grep -c " $pre/semillas/pred_.*\.txt$")
      ult=$(echo "$listado" | grep " $pre/" | awk '{print $1}' | sort | tail -1)
      echo "             a medias sp_$sp: BO $bo/$BO_TOTAL, semillas $sem/$SEMILLAS, ultimo respaldo $(hace "$ult")"
    done
  done
done

echo
echo "== Total (juntando buckets)"
tot=0
for e in $(printf '%s\n' "${!terminadas[@]}" | sort); do
  n=$(echo ${terminadas[$e]} | tr ' ' '\n' | grep -v '^$' | sort -u | wc -l)
  tot=$((tot + n))
  printf "   %-9s %2d/%d\n" "$e" "$n" "$CORRIDAS"
done
echo "   todo      $tot/$(( 6 * CORRIDAS ))  (Exp_0 + 5 ramas)"
