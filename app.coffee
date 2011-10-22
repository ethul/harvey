express = require "express"
Redis = require('connect-redis')(express)
app = module.exports = express.createServer()

app.configure ->
  app.set "views", "#{__dirname}/views"
  app.set "view engine", "jade"
  app.use express.bodyParser()
  app.use express.methodOverride()
  app.use express.static("#{__dirname}/public")
  app.use express.cookieParser()
  app.use express.session(secret: "somethingsecret", store: new Redis(), cookie: {maxAge: 60000})
  app.use app.router

app.configure "development", -> app.use express.errorHandler(dumpExceptions: true, showStack: true)
app.configure "production", -> app.use express.errorHandler()

# routes
app.get "/", (req,res) ->
  req.session.views++
  res.render "index", title: "express views #{req.session.views}"

app.listen 3000
console.log "express server listening on port #{app.address().port} in #{app.settings.env} mode"
