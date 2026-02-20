# 生成 manifest.json 文件
# 用于 Connect Cloud 部署

library(rsconnect)

# 为 Shiny 应用生成 manifest.json
# 默认会包含所有文件
rsconnect::writeManifest(
  appDir = ".",
  appPrimaryDoc = "app.R"
)

cat("manifest.json 文件已生成！\n")
