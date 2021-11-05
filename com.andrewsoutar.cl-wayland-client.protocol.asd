(asdf:defsystem #:com.andrewsoutar.cl-wayland-client.protocol
  :defsystem-depends-on (#:com.andrewsoutar.cl-wayland-client.generator)
  :class "COM.ANDREWSOUTAR.CL-WAYLAND-CLIENT.GENERATOR:PROTOCOL-SYSTEM"
  :depends-on (#:com.andrewsoutar.cl-wayland-client/core))
