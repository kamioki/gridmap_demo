#必要なパッケージをインストールします。
#この3行目は、初回実行時のみでOKです。
install.packages(c("openxlsx", "sf", "rmapshaper", "dplyr","ggplot2","ggforce","jpmesh"))

#インストールしたパッケージを読み込みます。
library(openxlsx)
library(sf)
library(rmapshaper)
library(dplyr)
library(ggplot2)
library(ggforce)
#jpgridパッケージの方が高速で海上のメッシュも描画できますが、今回はこちらを使います。
library(jpmesh)

#作業用フォルダの場所を指定します。今回はCドライブ直下に作ったRdataです。
#違う場所にデータを格納した場合は、ご自身の設定に合わせてフォルダの場所を指定してください。
setwd("C:/Rdata")

#Rdataフォルダの中にある観察データのエクセルを読み込み、kansatsuという名前にします。
#ファイル名はご自身のものに変更してください。
kansatsu <- read.xlsx("data.xlsx")

#「愛媛」フォルダの中にある行政区域データを読み込み、map_shapeという名前にします。
#フォルダ名はご自身のものに変更してください。
map_shape <- read_sf("愛媛", options = "ENCODING=SHIFT-JIS") %>%
  #N03_004列に市町村名が入力されているので、分かりやすくcityと名前を変更します。
  rename(city = N03_004)

#city名を基準に統合し、map_shapeのデータを「mapdayo」にまとめます。
mapdayo <- aggregate(map_shape, list(map_shape$city), unique) %>%
  #必要なcity列とgeometry列のデータのみを選択します。
  select(city, geometry) %>%
  #rmapshaperの機能で境界線を単純化し、地図の出力を高速化します。値は自由に変えてみてください。
  ms_simplify(keep = 0.005)

#地図が表示されれば成功！
plot(mapdayo)

# 緯度経度と市町村別地図の作成 -------------------------------------------------------------

#先ほどのmapdayoとkansatsuのデータを結合し、それをkansatsu_mapという名前のデータにします。
kansatsu_map <- left_join(kansatsu, mapdayo, by = c("市町" = "city"))

#kansatsu_mapから地図を描画します。
kansatsu_map %>%
  #特定の種類のみ表示したい場合はフィルターできます。ここでは全種表示したいので実行しません。
  #filter(grepl("カッパ",種名)) %>%
  #種名と市町の列でグループ化します（これはサンプルデータの列名。適宜変更してください）
  group_by(種名,市町)%>%
  #種ごとに、各市町の記録が何回あるかカウントし、新たなデータ列（市町別記録数）を追加します。
  mutate(市町別記録数 = n()) %>%
  #インストールしたパッケージggplotで地図を描画します。
  ggplot() +
  #mapdayoのデータで地図を描画します。地図は白色にします。
  geom_sf(data = mapdayo, fill = "white") + 
  #先ほど追加した市町別記録数の列のデータを基に色付け(fill)をします。
  geom_sf(aes(fill = 市町別記録数, geometry = geometry)) +
  #グラデーションで表現。少なければ黄色（yellow）多ければ深緑（#009933）にする。
  #色はカラーコードで細かく指定可能です。
  scale_fill_continuous(low = "yellow", high = "#009933") +
  #サンプルデータにある緯度経度の情報を表示。実際の経度や緯度の列名を指定してください。
  #Xが経度、Yが緯度になります。逆だと変な図が出力されるので注意。
  geom_point(aes(x = 経度, y = 緯度),
             #地図上の点のサイズと色、重なりを表すため透過率を指定。数字や色は好みの値に変更できます。
             size = 2, color = "black", alpha = 0.25) +
  #複数種のデータがあるので、種名ごとに地図を分けて表示します。
  facet_wrap(~種名) +
  #背景に緯度経度が表示されるので、すっきりさせるために白紙にします。
  theme_void()


# メッシュ地図の作成 -------------------------------------------------------------

#2次メッシュ（10km四方）のデータを作成します。codeの数字は都道府県コードです。 
#愛媛にしたいので38、メッシュサイズ(km)は10を指定します。 
mesh10map <- administration_mesh(code = 38, to_mesh_size = 10) 

#サンプルデータのメッシュは数値、mesh10mapのメッシュは文字列なので結合できるよう変換します。
kansatsu$メッシュ <- as.character(kansatsu$メッシュ)

#mesh10mapとkansatsuデータを、メッシュ番号を基準に結合してkansatsu_meshを作ります。
kansatsu_mesh <- left_join(kansatsu,mesh10map, by = c("メッシュ" = "meshcode"))

#kansatsu_meshから地図を描画します。
kansatsu_mesh %>%
  #種名とメッシュの列でグループ化（これは元データの列名。適宜変更してください）
  group_by(種名,メッシュ)%>%
  #種ごとに、各メッシュの記録が何回あるかRでカウントし、新たなデータ列（メッシュ数）を追加します。
  mutate(メッシュ数 = n()) %>%
  ggplot() +
  geom_sf(data = mapdayo, fill = "white") +
  #mesh10mapのメッシュを描画します。alpha=0で透過させ、線はグレーにします。
  geom_sf(data = mesh10map, alpha = 0, color = "grey") +
  #先ほど追加したメッシュ数の列のデータを基に色付け(fill)をします。
  geom_sf(aes(fill = メッシュ数, geometry = geometry)) +
  scale_fill_continuous(low = "yellow", high = "#009933") +
  #種名ごとに地図を分け、1列で表示します
  facet_wrap(~ 種名, nrow = 1) +
  theme_void()



# 種をフィルターした、年別地図の作成 -------------------------------------------------------------

kansatsu_mesh %>%
  filter(grepl("カッパ", 種名)) %>%
  #種名+メッシュ+年でグループ化してカウント
  group_by(種名, メッシュ, 年) %>%
  mutate(メッシュ数 = n()) %>%
  ggplot() +
  geom_sf(data = mapdayo, fill = "white") +
  geom_sf(data = mesh10map, alpha = 0, color = "grey") +
  geom_sf(aes(fill = メッシュ数, geometry = geometry)) +
  scale_fill_continuous(low = "yellow", high = "#009933") +
  #facet_wrap(年 ~ 種名)でもOK。グラフのタイトル順（優先カテゴリ）が変わります。
  #group_byで細かく年+月別にカウントした場合は 種名~年+月 などと + を付けて条件を増やせます。
  facet_wrap(種名 ~ 年, nrow = 2) +
  theme_void()



# 地図の一括出力 -------------------------------------------------------------

nanmai <- kansatsu_mesh %>%
  group_by(種名,市町,年,日) %>%
  mutate(メッシュ数 = n()) %>%
  ggplot() +
  geom_sf(data = mapdayo, fill = "white") +
  geom_sf(data = mesh10map, alpha = 0, color = "grey") +
  geom_sf(aes(fill = メッシュ数, geometry = geometry)) +
  scale_fill_continuous(low = "yellow", high = "#009933") +
  facet_wrap_paginate(種名 ~ 年, nrow = 2, ncol = 4, page = 3) +
  theme_void()

#地図を表示して出力結果を確認（page = 3としたので3ページ目が出力されます）
nanmai

#全体のページ数を確認（下のコンソールに[1] 5と出るはずです。つまり5ページです）
n_pages(nanmai)

#まずは空のPDFを作成します
pdf("map_zenbudayo.pdf", family="Japan1GothicBBB")

#そのPDFに n_pages(nanmai) のページ数分プリントします
for (i in seq_len(n_pages(nanmai))) {print(
  
  kansatsu_mesh %>%
    group_by(種名, メッシュ, 年) %>%
    mutate(メッシュ数 = n()) %>%
    ggplot() +
    geom_sf(data = mapdayo, fill = "white") +
    geom_sf(data = mesh10map, alpha = 0, color = "grey") +
    geom_sf(aes(fill = メッシュ数, geometry = geometry)) +
    scale_fill_continuous(low = "yellow", high = "#009933") +
    # page = のところは数字ではなく i とする。
    facet_wrap_paginate(種名~年, nrow = 2, ncol = 4, page = i) +
    theme_void()
  
)}
#PDFへのプリントを停止するため、以下のコマンドも必ず実行してください。
dev.off()