import SimpleJSON

result :: JValue
result = JObject [
    ("query", JString "awk squad"),
    ("estimatedCount", JNumber 3920),
    ("moreResults", JBool True),
    ("results", JArray [
        JObject [
            ("title", JString "Simon Peyton Jones: papers"),
            ("snippet", JString "Tackling the awkward ..."),
            ("url", JString "http://.../marktoberdorf/")
            ]])
    ]
