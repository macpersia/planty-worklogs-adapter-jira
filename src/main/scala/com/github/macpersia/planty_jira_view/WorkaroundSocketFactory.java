package com.github.macpersia.planty_jira_view;

import org.apache.commons.httpclient.params.HttpConnectionParams;
import org.apache.commons.httpclient.protocol.ProtocolSocketFactory;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.net.SocketFactory;
import javax.net.ssl.SSLSocketFactory;
import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.util.Optional;

/**
 * A workaround for https://forums.openshift.com/commons-httpclient-permission-denied.
 */
public class WorkaroundSocketFactory implements ProtocolSocketFactory {

    enum Protocol {
        HTTP("http"),
        HTTPS("https");

        public final String code;

        Protocol(String code) {
            this.code = code;
        }
    }

    final Protocol protocol;
    final Optional<String> localHostOverride;
    final Optional<Integer> localPortOverride;

    private static final Log LOG = LogFactory.getLog(WorkaroundSocketFactory.class);

    public WorkaroundSocketFactory(Protocol protocol,
                                   Optional<String> localHostOverride,
                                   Optional<Integer> localPortOverride) {
        this.protocol = protocol;
        this.localHostOverride = localHostOverride;
        this.localPortOverride = localPortOverride;
    }

    @Override
    public Socket createSocket(String host, int port,
                               InetAddress localAddress, int localPort)
            throws IOException {

        if (LOG.isDebugEnabled()) {
            LOG.debug("createSocket called. host = " + host + ", port = " + port
                    + (localHostOverride.isPresent() ? ""
                        : ", overriding localAddress = " + ((localAddress != null) ? localAddress.toString() : "null")
                            + " as " + localHostOverride)
                    + (localPortOverride.isPresent() ? ""
                        : ", overriding localPort = " + localPort
                            + " as " + localPortOverride));
        }
        try {
            LOG.debug("Socket created");
            SocketFactory factory = (protocol == Protocol.HTTPS) ?
                    SSLSocketFactory.getDefault()
                    : SocketFactory.getDefault();
//            return factory.createSocket(
//                    host, port,
//                    InetAddress.getByName(localHostOverride.orElse("localhost")),
//                    localPortOverride.orElse(0));
            return factory.createSocket(
                    host, port);

        } catch (IOException e) {
            LOG.error("Error creating socket: " + e.getMessage());
            throw e;
        }
    }

    @Override
    public Socket createSocket(String host, int port, InetAddress localAddress,
                               int localPort, HttpConnectionParams params)
            throws IOException {

        LOG.debug("createSocket called with HttpConnectionParams -- ignoring the timeout code and proceeding");
        return this.createSocket(host, port, localAddress, localPort);
    }

    @Override
    public Socket createSocket(String host, int port) throws IOException {

        LOG.debug("createSocket called with just host and port. proceeding..");
        return this.createSocket(host, port, null, 0);
    }

    @Override
    public boolean equals(Object obj) {
        return !(obj instanceof WorkaroundSocketFactory) ? false
                : ((WorkaroundSocketFactory) obj).protocol == protocol;
    }

    @Override
    public int hashCode() {
        return protocol.hashCode();
    }
}