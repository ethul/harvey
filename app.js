(function() {
  var Redis, app, express;
  express = require("express");
  Redis = require('connect-redis')(express);
  app = module.exports = express.createServer();
  app.configure(function() {
    app.set("views", "" + __dirname + "/views");
    app.set("view engine", "jade");
    app.use(express.bodyParser());
    app.use(express.methodOverride());
    app.use(express.static("" + __dirname + "/public"));
    app.use(express.cookieParser());
    app.use(express.session({
      secret: "somethingsecret",
      store: new Redis(),
      cookie: {
        maxAge: 60000
      }
    }));
    return app.use(app.router);
  });
  app.configure("development", function() {
    return app.use(express.errorHandler({
      dumpExceptions: true,
      showStack: true
    }));
  });
  app.configure("production", function() {
    return app.use(express.errorHandler());
  });
  app.get("/", function(req, res) {
    req.session.views++;
    return res.render("index", {
      title: "express views " + req.session.views
    });
  });
  app.listen(3000);
  console.log("express server listening on port " + (app.address().port) + " in " + app.settings.env + " mode");
}).call(this);
