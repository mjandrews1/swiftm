// swift-tools-version:5.10
import PackageDescription

let package = Package(
    name: "swiftm",
    platforms: [.macOS(.v14)],
    products: [
        .library(name: "SwiftM", targets: ["IR", "parser", "storage", "runtime"]),
        .executable(name: "swiftm", targets: ["cli"])
    ],
    targets: [
        .target(name: "IR", path: "Sources/IR"),
        .target(name: "storage", path: "Sources/storage"),
        .target(name: "parser", dependencies: ["IR"], path: "Sources/parser"),
        .target(name: "runtime", dependencies: ["IR", "storage", "parser"], path: "Sources/runtime"),
        .executableTarget(name: "cli", dependencies: ["runtime", "parser", "IR"], path: "Sources/cli"),
        .testTarget(name: "SwiftMTests", dependencies: ["runtime"], path: "Tests")
    ]
)
